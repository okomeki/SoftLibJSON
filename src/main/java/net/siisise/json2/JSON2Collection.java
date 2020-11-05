package net.siisise.json2;

/**
 *
 * @param <T>
 */
public interface JSON2Collection<T> extends JSON2Value {
    
    JSON2Value getJSON(String key);
    void setJSON(String key, JSON2Value obj);
    void addJSON(String key, JSON2Value obj);
    /**
     * 戻り型がJSON2Valueな違い
     * @param key キー
     * @param obj データ
     * @return 
     */
    JSON2Value putJSON(String key, JSON2Value obj);
    /**
     * 戻り型がJSON2Valueなremove.
     * keyに一致する項目を削除して返す.
     * @param key
     * @return 該当しない場合はnullかな
     */
    JSON2Value removeJSON(String key);
    
    /**
     * プリミティブを含む配列、Collection、などに変換する。
     * 何にでも変換できるといい。
     * JSONArrayからList<String> にしたい場合 List.class, String.class で変換される。
     * JSONObjectからMap<String,Example> の場合 Map.class, String.class, Example.class と指定する。
     * Collection以外(class,array)の内部要素は変数型から判定するので別途指定は不要。
     * 省略時、不足する場合はJSONValueから変換しない。
     * Typeを使用したtypeMap(Type) がおすすめかもしれない。
     * 
     * @deprecated #typeMap(Type) へ移行する
     * @param <T>
     * @param clss
     * @return
     */
    <T> T map(Class... clss);

    default String tab(String val) {
        return val.replace("\r\n", "\r\n  ");
    }
    
}
