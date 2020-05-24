package net.siisise.json;

import java.util.Map;

/**
 * JSON中間形式なinterface.
 * 
 * STD 90
 * RFC 8259
 * https://tools.ietf.org/html/rfc8259
 * ECMA-404 2nd Edition
 * 書式の情報は保存しない
 * 
 * Java API for JSON Processing (JSR-353),
 * JSON Processing (JSR-374),JSON-B (JSR-367)相当の機能を持っているが、API準拠はしていない。
 *
 * https://datatracker.ietf.org/wg/json/documents/
 * @param <T>
 */
public interface JSON<T> {

    /**
     * JSON型のもの
     *
     * @return
     */
    T value();

    /**
     * List,Map,String,Integer,Booleanなど各要素に変換したもの
     *
     * @return
     */
    Object map();

    /**
     * String, プリミティブ型、プリミティブ対応型ぐらいに変形できるといい
     * @param <E>
     * @param cls
     * @return
     */
    <E> E map(Class<E> cls);
    
    /**
     *
     * @param <E>
     * @param replaces
     * @param cls
     * @return
     */
    <E> E map(Map<Class,JSONReplaceMO> replaces, Class<E> cls);

    /**
     * JSON テキスト出力
     *
     * @return
     */
    @Override
    String toString();
    
    public static final JSONFormat NOBR = new JSONFormat("","");
    public static final JSONFormat TAB = new JSONFormat("\r\n","  ");

    /**
     * JSON テキスト出力
     * @param format
     * @return 
     */
    String toString(JSONFormat format);
    
    /**
     * JSON から中間形式に変換する.
     * @param json テキスト型JSON
     * @return JSONオブジェクト
     */
    public static JSONValue parse(String json) {
        return JSON8259Reg.parse(json);
    }
    
    /**
     * JSON式を中間形式に変換する.
     * @param json utf-8のデータ
     * @return JSONオブジェクト
     */
    public static JSONValue parse(byte[] json) {
        return JSON8259Reg.parse(json);
    }
    
    /**
     * 変換方法のひとつ
     * @param json JSON文字列
     * @param target 変換対象クラス
     * @return オブジェクト
     */
    public static <E> E parseToObj(String json, Class<E> target) {
        return (E)parse(json).map(target);
    }
    
    /**
     * 
     * @param obj オブジェクト
     * @return JSON文字列 
     */
    public static String stringify(Object obj) {
        return JSONValue.valueOf(obj).toString();
    }

    public static String stringify(Object obj, JSONReplacer r) {
        return JSONValue.valueOf(obj).toString();
    }
    /*
    public static String stringify(Object o, String[] replacer) {
        return JSONValue.valueOf(o).toString();
    }
    
    public static String stringify(Object o, Object nu, int tab) {
        throw new java.lang.UnsupportedOperationException();
    }*/
}
