/*
 * Copyright 2022 okome.
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
package net.siisise.json2;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.BiConsumer;
import java.util.stream.Collectors;
import javax.json.JsonObject;
import javax.json.JsonValue;
import net.siisise.json2.jsonp.JSONPObject;
import net.siisise.omap.OMAP;

/**
 * JavaではMapに相当する中間型。
 * typeMapによりObjectとも対応する。
 * JsonValue以外の型でも持てるのでJSONPに適さない
 * JSONPではBuilder系かもしれない
 *
 * @param <V> JSONに変換可能な型、または自由
 */
public class JSON2Object<V> extends LinkedHashMap<String, V> implements JSON2Collection<V> {

    public JSON2Object() {
    }

    public JSON2Object(Map<?,V> map) {
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
    public JSON2Value getJSON(String key) {
        if ( !containsKey(key)) {
            return null;
        }
        return JSON2.valueOf(get(key));
    }

    @Override
    public void setJSON(String key, JSON2Value obj) {
        putJSON(key, obj);
    }

    @Override
    public void addJSON(String key, JSON2Value obj) {
        putJSON(key, obj);
    }

    @Override
    public JSON2Value putJSON(String key, JSON2Value obj) {
        return JSON2.valueOf(put(key, obj.map()));
    }

    @Override
    public JSON2Value removeJSON(String key) {
        if (containsKey(key)) {
            return JSON2.valueOf(remove(key));
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
        return toString(NOBR);
    }

    @Override
    public String toString(JSON2Format format) {
        return keySet().stream().map(key -> {
            return format.crlf + format.tab + new JSON2String(key).toString(format) + ":"
                    + tab(getJSON(key).toString(format));
        }).collect(Collectors.joining(",", "{", format.crlf + "}"));
    }

    /**
     * 雑な複製対応。
     * 可変なので複製する。
     * @return 中まで複製したもの
     */
    @Override
    public JSON2Object<V> clone() {
        JSON2Object<V> clone = (JSON2Object<V>) super.clone();
        clone.clear();
        for ( Map.Entry<String, V> e : entrySet() ) {
            V v = e.getValue();
            if ( v == null ) {
            } else if ( v instanceof ArrayList ) { // JSON2Array の素
                v = (V)((ArrayList)v).clone();
            } else if ( v instanceof HashMap ) { // JSON2Object の素
                v = (V)((HashMap)v).clone();
            } else if ( v instanceof JSON2Value || v instanceof JsonValue ) { // 複製不要
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
    void j2forEach(BiConsumer<String,? super JSON2Value> action) {
        forEach((k,v)->{ action.accept(k, JSON2.valueOf(v));});
    }
}
