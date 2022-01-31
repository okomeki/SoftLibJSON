/*
 * Copyright 2022 Siisise Net
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
package net.siisise.json;

import java.lang.reflect.Type;
import java.util.stream.IntStream;
import javax.json.JsonString;
import javax.json.JsonValue;
import net.siisise.abnf.AbstractABNF;
import net.siisise.io.FrontPacket;
import net.siisise.lang.CodePoint;
import net.siisise.json.bind.OMAP;

/**
 * JSON String.
 */
public class JSON2String implements JSON2Value,JsonString,CharSequence {
    
    private final String value;

    public JSON2String(CharSequence val) {
        value = val.toString();
    }

    public JSON2String(String val) {
        value = val;
    }

    @Override
    public <T> T map() {
        return (T)value;
    }

    @Override
    public <T> T typeMap(Type type) {
        if ( type == JsonString.class || type == JsonValue.class ) {
            return (T) toJson();
        }
        return (T)OMAP.typeString(value, type);
    }

    @Override
    public JsonValue toJson() {
        return this;
    }
    
    @Override
    public String toString() {
        return value;
    }

    @Override
    public String toJSON() {
        return toJSON(NOBR);
    }

    @Override
    public String toJSON(JSON2Format format) {
        return "\"" + esc(value) + "\"";
    }
    
    private static String esc(String val) {
        StringBuilder sb = new StringBuilder();
        FrontPacket pac = AbstractABNF.pac(val);
        int ch;
        while (pac.length() > 0) {
            ch = CodePoint.utf8(pac);
            switch (ch) {
                case 0x22:
                case 0x2f:
                case 0x5c:
                    sb.append((char) 0x5c);
                    sb.append((char) ch);
                    break;
                case 0x08:
                    sb.append((char) 0x5c);
                    sb.append((char) 0x62);
                    break;
                case 0x0c:
                    sb.append((char) 0x5c);
                    sb.append((char) 0x66);
                    break;
                case 0x0a:
                    sb.append((char) 0x5c);
                    sb.append((char) 0x6e);
                    break;
                case 0x0d:
                    sb.append((char) 0x5c);
                    sb.append((char) 0x72);
                    break;
                case 0x09:
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
                    } else */ if (ch < 0x20) {
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
    public String getString() {
        return value;
    }

    @Override
    public CharSequence getChars() {
        return value;
    }
    
    @Override
    public boolean equals(Object obj) {
        if ( obj instanceof JsonString) {
            return value.equals(((JsonString) obj).getString());
        }
        return false;
    }

    @Override
    public ValueType getValueType() {
        return ValueType.STRING;
    }

    public int length() {
        return value.length();
    }

    @Override
    public char charAt(int index) {
        return value.charAt(index);
    }

    @Override
    public CharSequence subSequence(int start, int end) {
        return value.subSequence(start, end);
    }
    
    @Override
    public IntStream chars() {
        return value.chars();
    }
    
    @Override
    public IntStream codePoints() {
        return value.codePoints();
    }
}
