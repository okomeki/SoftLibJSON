package net.siisise.json;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.logging.Level;
import java.util.logging.Logger;
import net.siisise.abnf.parser.ABNFBaseParser;
import net.siisise.json.parser.JSONArrayP;
import net.siisise.json.parser.JSONNumberP;
import net.siisise.json.parser.JSONObjectP;
import net.siisise.json.parser.JSONStringP;
import net.siisise.json.parser.JSONValueP;

/**
 * 基本型
 * @param <T>
 */
public abstract class JSONValue<T> implements JSON<T> {

    T value;

    @Override
    public T value() {
        return value;
    }
    
    @Override
    public String toString() {
        return toString(TAB);
    }

    /**
     * 改行を少しなんとかする。
     * @param format TABかNOBR
     * @return 
     */
    @Override
    public String toString(JSONFormat format) {
        return value.toString();
    }

    String tab(String val) {
        return val.replace("\r\n", "\r\n  ");
    }
    
    /**
     * ParserにstaticでvalueOfを実装してみる
     * Replacer としてあとでまとめる
     */
    static Class[] PARSERS = {
        JSONValueP.class,
        JSONNumberP.class,
        JSONStringP.class,
        JSONArrayP.class,
        JSONObjectP.class
    };
    
    public static JSONValue valueOf(Object src) {
        return valueOf(src, null);
    }

    /**
     * なんでもJSONに変換する。
     * プリミティブ型、配列、Collection、Object boolean byte short char int long float
     * double List Map Number null String
     * Date型など要検討
     * @param src データ型なんでも
     * @param replacer
     * @return JSONValue
     */
    public static JSONValue valueOf(Object src, JSONReplacer replacer) {
        for ( Class<? extends ABNFBaseParser> parserClass : PARSERS ) {
            try {
                Method convert = parserClass.getMethod("valueOf", Object.class, JSONReplacer.class);
                JSONValue val = (JSONValue)convert.invoke(null, new Object[] {src, replacer});
                if ( val != null ) {
                    return val;
                }
            } catch (NoSuchMethodException ex) {
                Logger.getLogger(JSONValue.class.getName()).log(Level.SEVERE, null, ex);
            } catch (SecurityException ex) {
                Logger.getLogger(JSONValue.class.getName()).log(Level.SEVERE, null, ex);
            } catch (IllegalAccessException ex) {
                Logger.getLogger(JSONValue.class.getName()).log(Level.SEVERE, null, ex);
            } catch (IllegalArgumentException ex) {
                Logger.getLogger(JSONValue.class.getName()).log(Level.SEVERE, null, ex);
            } catch (InvocationTargetException ex) {
                Logger.getLogger(JSONValue.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return JSONObject.convObject(src);
    }

    /**
     *
     * @param o
     * @return
     */
    @Override
    public boolean equals(Object o) {
        return o != null && getClass() == o.getClass()
                && ((value == null && ((JSONValue) o).value == null) || value.equals(((JSONValue) o).value));
    }
}
