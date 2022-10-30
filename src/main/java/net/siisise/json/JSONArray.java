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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.json.JsonArray;
import javax.json.JsonValue;
import net.siisise.json.jsonp.JSONPArray;
import net.siisise.json.bind.OMAP;
import net.siisise.json.jsonxp.JSONXArray;

/**
 * Listを拡張したJSONArray。
 * 一般のデータを保持してJSONにも変換可能なスタイル。
 * 配列、Listの他ObjectのコンストラクタにもtypeMap可能。
 * JSONP準拠のものはEをJsonValueにするといい。
 *
 * j2Stream() を使うとJSON2Valueとして容易に扱える
 *
 * JsonArray,JsonArrayBuilder,JsonStructure ではない
 *
 * @param <E> 内部で保持する型。JSONではなくていい。
 */
public class JSONArray<E> extends ArrayList<E> implements JSONCollection<E> {

    /**
     * Eを参照したいので持っておく型.
     * JsonArray用かもしれない
     */
    private final Class<E> def;

    public JSONArray() {
        def = null;
    }

    protected JSONArray(Class<E> c) {
        def = c;
    }

    public JSONArray(Collection<E> vals) {
        super(vals);
        def = null;
    }

    @Override
    public JSONValue getJSON(String key) {
        return getJSON(Integer.parseInt(key));
    }

    public JSONValue getJSON(int index) {
        return JSON.valueOf(get(index));
    }

    @Override
    public void setJSON(String key, JSONValue obj) {
        E val = def == null ? obj.map() : obj.typeMap(def);
        if (key.equals("-")) {
            add(val);
        } else {
            set(Integer.parseInt(key), val);
        }
    }

    public void setJSON(int index, JSONValue obj) {
        E val = def == null ? obj.map() : obj.typeMap(def);
        set(index, val);
    }

    @Override
    public void addJSON(String key, JSONValue obj) {
        E val = def == null ? obj.map() : obj.typeMap(def);
        if (key.equals("-")) {
            add(val);
        } else {
            add(Integer.parseInt(key), val);
        }
    }

    public void addJSON(int index, JSONValue obj) {
        E val = def == null ? obj.map() : obj.typeMap(def);
        add(index, val);
    }

    /**
     * 指定位置のデータを配列から取り除く
     *
     * @param key 配列のindex相当文字列
     * @return 削除した値
     */
    @Override
    public JSONValue removeJSON(String key) {
        return JSON.valueOf(remove(Integer.parseInt(key)));
    }

    @Override
    public JSONValue putJSON(String key, JSONValue obj) {
        JSONValue val = getJSON(key);
        setJSON(key, obj);
        return val;
    }

    /**
     * primitive に変える
     *
     * @param val
     */
    public void addValue(Object val) {
        if (def != null) {
            val = OMAP.valueOf(val, def);
        } else {
            val = JSON.valueMap(val); // Object系に変換
        }
        add((E) val);
    }

    /**
     * JSON Bの fromJson相当.
     * @param <T>
     * @param type
     * @return 
     */
    @Override
    public <T> T typeMap(Type type) {
        return OMAP.typeList(this, type);
    }

    /**
     * forEachもここから
     *
     * @return
     */
    Stream<JSONValue> j2Stream() {
        return parallelStream().map(JSON::valueOf);
    }

    /* まだ不要
    void j2forEach(java.util.function.Consumer<? super JSON2Value> action) {
        j2Stream().forEach(action);
    } */
    /**
     * データを適当な型に変換して納めるので変換可能な配列ならなんでもいい。
     *
     * @param <T> aと互換性があればいい
     * @param a 抽出したい型の配列 0または必要数
     * @return
     */
    @Override
    public <T> T[] toArray(T[] a) {
        Class contentType = a.getClass().getComponentType();
        if (def != null && contentType == def) {
            return (T[]) toArray();
        }
        return parallelStream().map(v -> OMAP.valueOf(v, contentType)).collect(Collectors.toList()).toArray(a);
    }

    /**
     * 変換補助. Java EE JSON2な
     *
     * @return JsonArrayに
     */
    @Override
    public JsonArray toJson() {
        if (isEmpty()) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }
        JSONPArray ar = new JSONPArray();
        parallelStream().map(v -> (JsonValue) OMAP.valueOf(v, JsonValue.class)).forEach(ar::add);
        return ar;
    }

    /**
     * 変換補助. Jakarta EE用に変更する予定.
     *
     * @return
     */
    @Override
    public javax.json.JsonArray toXJson() {
        if (isEmpty()) {
            return javax.json.JsonValue.EMPTY_JSON_ARRAY;
        }
        JSONXArray ar = new JSONXArray();
        parallelStream().map(v -> (javax.json.JsonValue) OMAP.valueOf(v, javax.json.JsonValue.class)).forEach(ar::add);
        return ar;
    }

    @Override
    public String toJSON() {
        return toJSON(NOBR);
    }

    @Override
    public String toString() {
        return toJSON();
    }

    @Override
    public String toJSON(JSONFormat format) {
        return j2Stream().map(val
                -> format.crlf + format.tab + tab(val.toJSON(format)))
                .collect(Collectors.joining(",", "[", format.crlf + "]"));
    }

    /**
     * ListっぽいJSON2Arrayの複製(表面のみ)で返す。
     *
     * @param <T>
     * @return
     */
    @Override
    public <T> T map() {
        return (T) new JSONArray(this);
    }

    /**
     * 雑な複製対応。
     * 可変なので複製する
     *
     * @return 配列、オブジェクトは中まで複製したもの
     */
    @Override
    public JSONArray<E> clone() {
        JSONArray<E> array = (JSONArray<E>) super.clone();
        array.clear();
        for (E e : this) {
            if (e == null) {
            } else if (e instanceof ArrayList) { // JSONArray っぽいもの
                e = (E) ((ArrayList) e).clone();
            } else if (e instanceof HashMap) { // JSONObject っぽいもの
                e = (E) ((HashMap) e).clone();
            } else if (e instanceof JSONValue || e instanceof JsonValue) { // 複製しなくていい

            } else if (e instanceof Cloneable) {
                try {
                    Method cl = e.getClass().getMethod("clone");
                    e = (E) cl.invoke(e);
                } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException ex) {
//                    Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
            array.add((E) e);
        }
        return array;
    }
}
