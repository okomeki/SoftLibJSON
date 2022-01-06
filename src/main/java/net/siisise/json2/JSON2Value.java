package net.siisise.json2;

import java.lang.reflect.Type;
import javax.json.JsonValue;

/**
 * 要素的なJSON。
 * 
 * JSON2Object implements Map 可変
 * JSON2Array implements List 可変
 * JSON2Number implements Number 不変
 * JSON2String 互換なし 不変
 * JSON2Boolean 互換なし 不変
 * JSON2NULL 互換なし 不変
 *
 * toString() をJSON出力にすること
 * JSON2ObjectとJSON2ArrayはMapとListとして内容の変更が可能
 * JSON2を継承する必要はなくなったのでどこかで外す。
 */
public interface JSON2Value extends JSON2 {
    
    /**
     * JSON (JavaのString)として出力する.
     * @return JSON文字列
     */
    @Override
    String toString();

    public static final JSON2Format NOBR = new JSON2Format("","");
    public static final JSON2Format TAB = new JSON2Format("\r\n","  ");

    /**
     * 書式を指定してJSONとして出力する.
     *
     * @param format
     * @return
     */
    String toString(JSON2Format format);

    /**
     * 固定のJava寄りの型に変換する。
     * List,Map,primitive対応データ型など
     * array, object, またはその中身は型変換が介入しない場合は複製されない
     * @param <T>
     * @return 
     */
    <T> T map();
    
    /**
     * 可能な限り指定型に変換する。
     * Fieldから
     * List,Map,配列,Java Object, Genericなどまで可
     * @param <T>
     * @param type Classの他、Fieldから取得できる型情報ParameterizedTypeに対応する
     * @return 
     */
    <T> T typeMap(Type type);
    
    /**
     * JSONP系に準拠する.
     * @return 
     */
    JsonValue toJson();

}
