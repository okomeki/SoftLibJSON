package net.siisise.json;

import java.lang.reflect.Type;
import java.util.stream.Collector;
import java.util.stream.Collector.Characteristics;
import javax.json.JsonValue;
import net.siisise.omap.OMAP;

/**
 * JSON中間形式なinterface.
 * 
 * STD 90
 * RFC 8259
 * https://tools.ietf.org/html/rfc8259
 * ECMA-404 2nd Edition
 * 書式の情報は保存しない
 * 
 * JSON String to JSON Value #parse(JSON Value)
 * String,Object,List etc to JSON Value #valueOf(Object)
 * JSON Value to Obj #map(Class...) / #typeMap(Class/Type)
 * JSON Value to JSON String  toString() / toString(TAB)
 * JSON Value to JSONP #toJson()
 * 
 * Java API for JSON Processing (JSR-353),
 * JSON Processing (JSR-374),JSON-B (JSR-367)相当の機能を持っているが、API準拠はしていない。
 *
 * https://datatracker.ietf.org/wg/json/documents/
 * @param <T>
 */
public interface JSON<T> {

    /**
     * 中
     *
     * @return
     */
    T value();

    /**
     * JSONB
     * List,Map,String,Integer,Booleanなど各要素に変換したもの
     *
     * @return
     */
    Object map();

    /**
     * 型情報からObject型を推測しながらマッピングする.
     * JSONBっぽいもの
     * String, プリミティブ型、プリミティブ対応型ぐらいに変形できるといい
     * @param <E>
     * @param type
     * @return
     */
    <E> E typeMap(Type type);
    
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
        return (E)parse(json).typeMap(target);
    }
    
    /**
     * 
     * @param obj オブジェクト
     * @return JSON文字列 
     */
    public static String stringify(Object obj) {
        return valueOf(obj).toString();
    }

    /**
     * @deprecated まだかもしれない
     * @param obj
     * @param r
     * @return 
     */
    public static String stringify(Object obj, JSONReplacer r) {
        return valueOf(obj).toString();
    }
    /*
    public static String stringify(Object o, String[] replacer) {
        return JSONValue.valueOf(o).toString();
    }
    */
    
    /**
     * なんでもJSONに変換する。
     * プリミティブ型、配列、Collection、Object boolean byte short char int long float
     * double List Map Number null String
     * Date型など要検討
     * @param src データ型なんでも
     * @param replacer
     * @return JSONValue
     */
    public static JSONValue valueOf(Object src) {
        return OMAP.valueOf(src, JSONValue.class);
    }

    /**
     * Java API for JSON Processing 系オブジェクトに変換するつもり.
     * @return Java API系 JsonValue
     */
    public JsonValue toJson();
    
    /**
     * 
     * @param <T>
     * @return 
     */
    public static <T> Collector<T,?,JSONArray> toJSONArray() {
        return Collector.of(
                JSONArray::new,
                JSONArray::add,
                (ls, vals) -> {
                    vals.forEach(ls::add);
                    return ls;
                },
                Characteristics.IDENTITY_FINISH
        );
    }
}
