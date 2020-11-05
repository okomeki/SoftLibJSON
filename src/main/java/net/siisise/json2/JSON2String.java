package net.siisise.json2;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Type;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonString;
import javax.json.JsonValue;
import net.siisise.abnf.AbstractABNF;
import net.siisise.io.Packet;
import net.siisise.json.JSONFormat;
import net.siisise.json.JSONString;
import net.siisise.lang.CodePoint;

/**
 *
 */
public class JSON2String implements JSON2Value {
    
    private final String value;

    JSON2String(String val) {
        value = val;
    }

    @Override
    public <T> T map() {
        return (T)value;
    }

    @Override
    public <T> T typeMap(Type type) {
        if ( type == String.class ) {
            return (T)value;
        } else if ( type == StringBuilder.class ) {
            return (T)new StringBuilder(value);
        } else if ( type == StringBuffer.class ) {
            return (T)new StringBuffer(value);
        } else if ( type == JsonString.class || type == JsonValue.class ) {
            return (T) toJson();
        }
        try {
            Constructor<T> c = ((Class)type).getConstructor(value.getClass());
            return c.newInstance(value);
        } catch (NoSuchMethodException ex) {
            Logger.getLogger(JSON2String.class.getName()).log(Level.SEVERE, null, ex);
        } catch (SecurityException ex) {
            Logger.getLogger(JSON2String.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
            Logger.getLogger(JSON2String.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            Logger.getLogger(JSON2String.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalArgumentException ex) {
            Logger.getLogger(JSON2String.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InvocationTargetException ex) {
            Logger.getLogger(JSON2String.class.getName()).log(Level.SEVERE, null, ex);
        }
        return (T)value;
    }

    @Override
    public JsonValue toJson() {
        return new JSONString(value);
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
}
