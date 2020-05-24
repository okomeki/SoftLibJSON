package net.siisise.json;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Map;
import javax.json.JsonString;
import javax.json.JsonValue;
import net.siisise.abnf.AbstractABNF;
import net.siisise.io.Packet;
import net.siisise.lang.CodePoint;

/**
 * 文字列.
 */
public class JSONString extends JSONValue<String> implements JsonString {
    
    public JSONString(String value) {
        this.value = value;
    }

    @Override
    public String map() {
        return value;
    }

    @Override
    public String toString(JSONFormat format) {
        return "\"" + esc(value) + "\"";
    }
    
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

    /**
     *
     * @param <E>
     * @param map
     * @param cls
     * @return
     */
    @Override
    public <E> E map(Map<Class,JSONReplaceMO> map, Class<E> cls) {
        JSONReplaceMO conv = map.get(cls);
        if ( conv != null ) {
            return (E) conv.replace(this, cls);
        }
        return (E) map(cls);
    }
    
    /**
     *
     * @param <T>
     * @param cls
     * @return
     */
    @Override
    public <T> T map(Class<T> cls) {
        if ( cls == String.class) {
            return (T)value;
        }
        try {
            Constructor<T> c = cls.getConstructor(value.getClass());
            return c.newInstance(value);
        } catch (NoSuchMethodException ex) {
//            Logger.getLogger(JSONString.class.getName()).log(Level.SEVERE, null, ex);
        } catch (SecurityException ex) {
//            Logger.getLogger(JSONString.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
//            Logger.getLogger(JSONString.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
//            Logger.getLogger(JSONString.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalArgumentException ex) {
//            Logger.getLogger(JSONString.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InvocationTargetException ex) {
//            Logger.getLogger(JSONString.class.getName()).log(Level.SEVERE, null, ex);
        }
        return (T) map();
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

    @Override
    public JsonValue toJson() {
        return this;
    }
}
