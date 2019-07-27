package net.siisise.json;

/**
 * RFC 8259
 * https://tools.ietf.org/html/rfc8259
 * 書式の情報は保存しない
 *
 * @author okome
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
     * @param <T>
     * @param cls
     * @return
     */
    <T> T map(Class<T> cls);

    /**
     * JSON テキスト出力
     *
     * @return
     */
    @Override
    String toString();
    
    public static JSONValue parse(String json) {
        return JSON8259Reg.parse(json);
    }
    
    /**
     * 変換方法のひとつ
     * @param json
     * @param target
     * @return 
     */
    public static Object parseToObj(String json, Class target) {
        return parse(json).map(target);
    }
    
    public static String stringify(Object o) {
        return JSONValue.valueOf(o).toString();
    }
    /*
    public static String stringify(Object o, JSONReplacer r) {
        //return JSONValue.valueOf(o).toString();
    }
    
    public static String stringify(Object o, String[] replacer) {
        return JSONValue.valueOf(o).toString();
    }
    
    public static String stringify(Object o, Object nu, int tab) {
        throw new java.lang.UnsupportedOperationException();
    }*/
    
}
