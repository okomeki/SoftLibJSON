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
     *
     * @param <E>
     * @param cls
     * @return
     */
    <E> E map(Class<E> cls);
    
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
    
    public static JSONValue parse(String json) {
        return JSON8259Reg.parse(json);
    }
    
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
