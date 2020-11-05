package net.siisise.json2;

import net.siisise.json.JSON;
import net.siisise.json.JSONFormat;
import net.siisise.json.JSONReplacer;

/**
 * 中間形式をListとMap対応にして実質なくした版
 * JSON文字列 toString()
 * JSON2Object valueOf()
 * List/Mapプリミティブ列 map()
 * JavaObjectマップ typeMap()
 * JSONP toJson()
 */
public interface JSON2 {
   
    static final JSON2Map PUBLIC = new JSON2Map();
    static JSON2Map PARSERS = PUBLIC;
    
    /**
     * JSON文字列からObjectにパースする.
     * 
     * @param json
     * @return 
     */
    static Object parse(String json) {
        return JSON28259Reg.parse(json);
    }

    /**
     * JSONデータ列からObjectにパースする
     * @param json
     * @return 
     */
    static Object parse(byte[] json) {
        return JSON28259Reg.parse(json);
    }

    public static JSONFormat NOBR = JSON.NOBR;
    public static JSONFormat TAB = JSON.TAB;
    
    String toString(JSONFormat format);
    
    /**
     * 
     * @param src
     * @return 
     */
    public static Object valueMap(Object src) {
        return valueMap(src, null);
    }
    
    /**
     * JSON中間型(風) Listまたは Map型で返す。 
     * @param src
     * @param replacer
     * @return JSON2系ListとMapのJavaっぽいデータ
     */
    public static Object valueMap(Object src, JSONReplacer replacer) {
        return PARSERS.valueOf(src, replacer);
    }
    
    /**
     * valueOf で結果が単体(primitive型)のときラップしてから返す
     * @param src
     * @return 
     */
    public static JSON2Value valueOf(Object src) {
        return valueWrap(valueMap(src, null));
    }
    
    static JSON2Value valueWrap(Object val) {
        if ( val == null ) {
            return JSON2NULL.NULL;
        } else if ( val instanceof JSON2Value ) {
            return (JSON2Value)val;
        } else if ( val instanceof Boolean ) {
            return (Boolean)val ? JSON2Boolean.TRUE : JSON2Boolean.FALSE;
        } else if ( val instanceof Number ) {
            return new JSON2Number((Number)val);
        } else if ( val instanceof String ) {
            return new JSON2String((String)val);
        }
        throw new UnsupportedOperationException("未");
    }
}
