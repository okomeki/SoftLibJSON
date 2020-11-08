package net.siisise.omap;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.siisise.json.JSON;

/**
 * SoftLibJSON から Object Mapping を分離する。
 * REST 準拠なデータを持つList(JSONArrayだったもの), Map(JSONObjectだったもの) から各種変換をする便利機能。
 * 
* @param <B>
 */
public class OMAP<B> extends HashMap<String,B> {
    
//    Map<String,B> value;
    
    public OMAP(Map<String,B> map) {
        super();
        super.putAll(map);
//        value = map;
    }
    
    /**
     * JSON Object 相当.
     * @param <T>
     * @param value
     * @param type
     * @return 
     */
    static <T> T typeMap(Map value, Type type) {
        if (type instanceof Class) {
            Class cls = (Class) type;
            
        } else if ( type instanceof ParameterizedType ) {
            
        }
        throw new UnsupportedOperationException("まだない");
    }
    
    /**
     * JSON Array 相当.7
     * @param <T>
     * @param value
     * @param type
     * @return 
     */
    static <T> T typeList(List value, Type type) {
        if (type instanceof Class) {
            Class cls = (Class) type;
            
        } else if ( type instanceof ParameterizedType ) {
            
        }
        
        throw new UnsupportedOperationException("まだない");
    }
    
    static <T> T typeString(String value, Type type) {
        throw new UnsupportedOperationException("まだない");
    }
    
     /**
     * あとで分ける.
     * @param <T>
     * @param value
     * @param type
     * @return 
     */
    public static <T> T typeValue(Object value, Type type) {
        if ( type instanceof Class ) {
            if ( type == String.class ) {
                return (T)JSON.valueOf(value).toString();
            } else if ( JSON.class.isAssignableFrom((Class) type) ) {
                JSON v = JSON.valueOf(value);
                if ( ((Class) type).isAssignableFrom(v.getClass())) {
                    return (T)v;
                } else {
                    throw new ClassCastException("JSONの型が不一致");
                }
            }
        }
        if ( value instanceof Map ) {
            return typeMap((Map)value, type);
        } else if ( value instanceof List ) {
            return typeList((List)value, type);
        } else if ( value instanceof String ) {
            typeString((String)value, type);
        }
        throw new UnsupportedOperationException("まだない");
    }
}
