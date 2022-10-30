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
package net.siisise.json.jsonxp;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONBoolean;
import net.siisise.json.base.JSONBaseNULL;

/**
 * 互換要素
 */
public class JSONXArray extends JSONArray<JsonValue> implements JsonArray {

    public JSONXArray() {
        super(JsonValue.class);
    }
    
    public JSONXArray(Collection<JsonValue> col) {
        super(col);
    }
    
    @Override
    public ValueType getValueType() {
        return ValueType.ARRAY;
    }

    @Override
    public boolean isNull(int i) {
        return get(i) == JsonValue.NULL || get(i) instanceof JSONBaseNULL;
    }

    @Override
    public JsonObject getJsonObject(int i) {
        return (JsonObject) get(i);
    }

    @Override
    public JsonArray getJsonArray(int i) {
        return (JsonArray) get(i);
    }

    @Override
    public JsonNumber getJsonNumber(int i) {
        return (JsonNumber) get(i);
    }

    @Override
    public JsonString getJsonString(int i) {
        return (JsonString) get(i);
    }

    @Override
    public String getString(int i) {
        return getJsonString(i).getString();
    }

    @Override
    public String getString(int i, String string) {
        JsonString v = getJsonString(i);
        return v == null ? string : v.getString();
    }

    @Override
    public int getInt(int i) {
        return getJsonNumber(i).intValue();
    }

    @Override
    public int getInt(int i, int i1) {
        JsonNumber val = getJsonNumber(i);
        return val == null ? i1 : val.intValue();
    }

    @Override
    public boolean getBoolean(int i) {
//        get(i).getValueType()
        return get(i) == JsonValue.TRUE || get(i) == JSONBoolean.TRUE;
    }

    @Override
    public boolean getBoolean(int i, boolean bln) {
        JsonValue val = get(i);
        return (val == null) ? bln : val == JsonValue.TRUE; 
    }

    /**
     *
     * @param <T>
     * @param type
     * @return
     */
    @Override
    public <T extends JsonValue> List<T> getValuesAs(Class<T> type) {
       // List<T> list = new ArrayList<>();
        return (List<T>)stream().collect(Collectors.toList());
        //this.forEach(val -> {
        //    list.add((T)val);
        //});
        //return list;
    }

    /**
     * JSONPArray互換の形式List&lt;JsonValue&gt;で格納する
     * @param <T> だいたいなんでもいけるかもしれない。
     * @return JSONP JsonArray対応型データ
     */
    public static <T> Collector<T,?,JSONXArray> collector() {
        return Collector.of(JSONXArray::new,
                JSONXArray::addValue,
                (ls, vals) -> {
                    vals.forEach(ls::addValue);
                    return ls;
                },
                Collector.Characteristics.IDENTITY_FINISH
        );
    }
}
