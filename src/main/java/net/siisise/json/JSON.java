package net.siisise.json;

/**
 * RFC 8259
 * https://tools.ietf.org/html/rfc8259
 * 書式の情報は保存しない
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
}
