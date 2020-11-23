package net.siisise.json2;

import java.lang.reflect.Type;
import javax.json.JsonString;
import javax.json.JsonValue;
import net.siisise.abnf.AbstractABNF;
import net.siisise.io.Packet;
import net.siisise.json.JSONFormat;
import net.siisise.lang.CodePoint;
import net.siisise.omap.OMAP;

/**
 *
 */
public class JSON2String implements JSON2Value,JsonString {
    
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
        return toString(NOBR);
    }

    @Override
    public String toString(JSONFormat format) {
        return "\"" + esc(value) + "\"";
    }
    
    /** JSONString と同じ */
    private static String esc(String val) {
        StringBuilder sb = new StringBuilder();
        Packet pac = AbstractABNF.pac(val);
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
    public ValueType getValueType() {
        return ValueType.STRING;
    }
}
