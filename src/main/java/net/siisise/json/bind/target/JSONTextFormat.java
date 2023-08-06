/*
 * Copyright 2023 okome.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.siisise.json.bind.target;

import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;
import net.siisise.bind.Rebind;
import net.siisise.bind.format.ContentBind;
import net.siisise.bind.format.TypeFormat;
import net.siisise.block.ReadableBlock;
import net.siisise.lang.CodePoint;

/**
 *
 */
public class JSONTextFormat implements TypeFormat<String>, ContentBind<String> {
    
    final String crlf;
    final String tab;
    final boolean max;
    
    public JSONTextFormat(String crlf, String tab, boolean escMax) {
        this.crlf = crlf;
        this.tab = tab;
        max = escMax;
    }
    
    public JSONTextFormat() {
        this("", "", true);
    }

    @Override
    public String contentType() {
        return "application/json";
    }

    /**
     * 最小限のエスケープのみにする。
     * 0x2f をエスケープしないだけ
     * 使用できない場面もあるかも
     * @param val
     * @return 
     */
    protected String esc(String val) {
        StringBuilder sb = new StringBuilder();
        ReadableBlock pac = ReadableBlock.wrap(val);
        int ch;
        while (pac.length() > 0) {
            ch = CodePoint.utf8(pac);
            switch (ch) {
                case 0x2f: // solidus / optional
                    if ( !max ) {
                        sb.append(ch);
                        break;
                    }
                case 0x22: // quotation mark " *必須
                case 0x5c: // reverse solidus \ *必須
                    sb.append((char) 0x5c);
                    sb.append((char) ch);
                    break;
                case 0x08: // backspace \b
                    sb.append((char) 0x5c);
                    sb.append((char) 0x62);
                    break;
                case 0x0c: // form feed \f
                    sb.append((char) 0x5c);
                    sb.append((char) 0x66);
                    break;
                case 0x0a: // line feed \n
                    sb.append((char) 0x5c);
                    sb.append((char) 0x6e);
                    break;
                case 0x0d: // carriage return \r
                    sb.append((char) 0x5c);
                    sb.append((char) 0x72);
                    break;
                case 0x09: // tab \t
                    sb.append((char) 0x5c);
                    sb.append((char) 0x74);
                    break;
                default:
                    /* if ( ch > 0xffff) {
                        char[] l = Character.toChars(ch);
                        String a = Integer.toHexString(0x10000 + l[0]).substring(1);
                        String b = Integer.toHexString(0x10000 + l[0]).substring(1);
                        sb.append((char)0x5c);
                        sb.append((char)0x75);
                        sb.append(a);
                        sb.append((char)0x5c);
                        sb.append((char)0x75);
                        sb.append(b);
                    } else */ if (ch < 0x20) { // escape 必須
                        String a = Integer.toHexString(0x10000 + ch).substring(1);
                        sb.append((char) 0x5c);
                        sb.append((char) 0x75);
                        sb.append(a);
                    } else {
                        sb.appendCodePoint(ch);
                    }
                    break;
            }
        }
        return sb.toString();
    }

    @Override
    public String nullFormat() {
        return "null";
    }

    @Override
    public String booleanFormat(boolean bool) {
        return Boolean.toString(bool);
    }

    /**
     * JSONではNaNやInfinityは扱えない.
     * @param num
     * @return 
     */
    @Override
    public String numberFormat(Number num) {
        if ( num instanceof Float && (((Float)num).isNaN() || ((Float)num).isInfinite())) {
            throw new IllegalStateException("JSON not support NaN of Infinitie");
        }
        if ( num instanceof Double && (((Double)num).isNaN() || ((Double)num).isInfinite())) {
            throw new IllegalStateException("JSON not support NaN or Infinitie");
        }
        return num.toString();
    }

    @Override
    public String stringFormat(String str) {
        return "\"" + esc(str) + "\"";
    }
    
    String tab(String src) {
        return src.replace("\r\n", "\r\n  ");
    }

    @Override
    public String mapFormat(Map map) {
        return (String) map.entrySet().stream().map(e -> {
            return ((String)Rebind.valueOf(((Map.Entry)e).getKey(), this)) + ":"
             + tab(Rebind.valueOf(((Map.Entry)e).getValue(), this));
        }).collect(Collectors.joining("," + crlf + tab, "{" + crlf + tab, crlf + "}"));
    }

    /**
     * list と set を統合したようなもの
     * @param col
     * @return 
     */
    @Override
    public String collectionFormat(Collection col) {
        return (String) col.stream().map(v -> tab(Rebind.valueOf(v, this)))
                .collect( Collectors.joining("," + crlf + tab, "{" + crlf + tab, crlf + "}"));
    }
}
