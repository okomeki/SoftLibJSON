package net.siisise.json2;

import java.lang.reflect.Type;
import javax.json.JsonValue;

/**
 * 
 * JSON2Object implements Map
 * JSON2Array implements List
 * JSON2Number implements Number
 * JSON2String 互換なし
 * JSON2Boolean 互換なし
 * JSON2NULL 互換なし
 */
public interface JSON2Value extends JSON2 {
    
    /**
     * 固定のJava寄りの型に変換する。
     * List,Map,primitive対応データ型など
     * @param <T>
     * @return 
     */
    <T> T map();

    /**
     * 可能な限り指定型に変換する。
     * List,Map,配列,Java Object, Genericなどまで可
     * @param <T>
     * @param type
     * @return 
     */
    <T> T typeMap(Type type);

    /**
     * JSONP系に準拠する.
     * @return 
     */
    JsonValue toJson();

}
