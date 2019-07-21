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
    
    public static JSONValue parse(String src) {
        return JSON8259Reg.value(src);
    }
    
    /**
     * 変換方法のひとつ
     * @param src
     * @return 
     */
    public static Object parseToObj(String src, Class target) {
        return parse(src).map(target);
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
