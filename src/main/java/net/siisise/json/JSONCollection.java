/*
 * Copyright 2022 Siisise Net
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.siisise.json;

/**
 * Collection (List),Mapを統合したような操作をJSON Pointerでできるような感じのもの。
 * Arrayではindexは文字列に変換して代用。挿入、削除すると配置は変わる。
 * @param <T> データ型
 */
public interface JSONCollection<T> extends JSONValue {
    
    /**
     * JSON2Value として内容を変換している
     * @param key
     * @return 
     */
    JSONValue getJSON(String key);

    /**
     * 値をセットする。
     * Array系のsetという名称、Object系のputJSONと同じ
     * @param key キー
     * @param obj
     */
    void setJSON(String key, JSONValue obj);

    /**
     * 
     * @param key 末尾に追加する場合は "-"
     * @param obj 
     */
    void addJSON(String key, JSONValue obj);

    /**
     * 値をセットする。
     * 戻り型がJSON2Valueな違い
     * @param key キー
     * @param obj データ
     * @return 
     */
    JSONValue putJSON(String key, JSONValue obj);
    /**
     * 戻り型がJSON2Valueなremove.
     * keyに一致する項目を削除して返す.
     * @param key
     * @return 該当しない場合はnullかな
     */
    JSONValue removeJSON(String key);
    
    default String tab(String val) {
        return val.replace("\r\n", "\r\n  ");
    }
    
    JSONCollection clone();
    
}
