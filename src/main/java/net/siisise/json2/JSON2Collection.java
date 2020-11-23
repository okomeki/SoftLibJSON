package net.siisise.json2;

/**
 * Collection (List),Mapを統合したような操作をJSON Pointerでできるような感じのもの
 * Arrayではindexは文字列に変換して代用。挿入、削除すると配置は変わる。
 * @param <T> データ型
 */
public interface JSON2Collection<T> extends JSON2Value {
    
    JSON2Value getJSON(String key);

    /**
     * 値をセットする。
     * Array系のsetという名称、Object系のputJSONと同じ
     * @param key
     * @param obj 
     */
    void setJSON(String key, JSON2Value obj);
    void addJSON(String key, JSON2Value obj);

    /**
     * 値をセットする。
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
    
    default String tab(String val) {
        return val.replace("\r\n", "\r\n  ");
    }
    
}
