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
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.BiConsumer;
import javax.json.JsonObject;
import javax.json.JsonValue;
import net.siisise.bind.format.TypeFormat;
import net.siisise.json.jsonp.JSONPObject;
import net.siisise.json.bind.OMAP;

/**
 * JSON Object.
 * JavaではMapに相当する中間型。
 * typeMapによりObjectとも対応する。
 * JsonValue以外の型でも持てるのでJSONPに適さない
 * JSONPではBuilder系かもしれない
 *
 * @param <V> JSONに変換可能な型、または自由
 */
public class JSONObject<V> extends LinkedHashMap<String, V> implements JSONCollection<V> {

    public JSONObject() {
    }

    public JSONObject(Map<?,V> map) {
        map.forEach((key, o) -> {
            if (key instanceof String) {
                put((String) key, o);
            } else {
                put(key.toString(), o);
            }
        });
    }

    /**
     * 値をJSON2Valueで返す
     * キーがある場合の値nullはJSON2NULL
     * @param key
     * @return JSON2Value または該当キーがない場合 null 
     */
    @Override
    public JSONValue getJSON(String key) {
        if ( !containsKey(key)) {
            return null;
        }
        return JSON.valueOf(get(key));
    }

    @Override
    public void setJSON(String key, JSONValue obj) {
        putJSON(key, obj);
    }

    @Override
    public void addJSON(String key, JSONValue obj) {
        putJSON(key, obj);
    }

    @Override
    public JSONValue putJSON(String key, JSONValue obj) {
        return JSON.valueOf(put(key, obj.map()));
    }

    @Override
    public JSONValue removeJSON(String key) {
        if (containsKey(key)) {
            return JSON.valueOf(remove(key));
        }
        return null;
    }
    
    /**
     * 入れ物なので複製しないで自身を返す
     * @return 自身を返す
     */
    @Override
    public HashMap<String, V> map() {
        return this;
    }

    /**
     * 特定の型情報に変換できる場合は変換する。
     * OMAP側に分けてある機能。
     * @param <T> 型
     * @param type 型情報
     * @return 変換されたオブジェクト
     */
    @Override
    public <T> T typeMap(Type type) {
        return OMAP.typeMap(this, type);
    }

    @Override
    public JsonObject toJson() {
        if (isEmpty()) {
            return JsonValue.EMPTY_JSON_OBJECT;
        } else {
            JSONPObject obj = new JSONPObject();
            forEach((k, v) -> {
                obj.put(k, OMAP.valueOf(v, JsonValue.class));
            });
            return obj;
        }
    }
    
    @Override
    public String toString() {
        return toJSON(NOBR_MINESC);
    }
    
    @Override
    public <T> T toJSON(TypeFormat<T> format) {
        return format.mapFormat(this);
    }

    /**
     * 雑な複製対応。
     * 可変なので複製する。
     * @return 中まで複製したもの
     */
    @Override
    public JSONObject<V> clone() {
        JSONObject<V> clone = (JSONObject<V>) super.clone();
        clone.clear();
        for ( Map.Entry<String, V> e : entrySet() ) {
            V v = e.getValue();
            if ( v == null ) {
            } else if ( v instanceof ArrayList ) { // JSON2Array の素
                v = (V)((ArrayList)v).clone();
            } else if ( v instanceof HashMap ) { // JSON2Object の素
                v = (V)((HashMap)v).clone();
            } else if ( v instanceof JSONValue || v instanceof JsonValue ) { // 複製不要
            } else if ( v instanceof Cloneable ) {
                try {
                Method cl = v.getClass().getMethod("clone");
                    v = (V) cl.invoke(v);
                } catch (NoSuchMethodException | SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
//                    Logger.getLogger(JSON2Object.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
            clone.put(e.getKey(), v);
        }
        return clone;
    }
    
    /**
     * value を JSON2Valueに変換したforEach
     * @param action 
     */
    void j2forEach(BiConsumer<String,? super JSONValue> action) {
        forEach((k,v)->{ action.accept(k, JSON.valueOf(v));});
    }
}
