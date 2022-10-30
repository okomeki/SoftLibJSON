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

import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import net.siisise.json.JSONObject;
import net.siisise.json.JSONPointer;

/**
 * 互換要素
 */
public class JSONXObject extends JSONObject<JsonValue> implements JsonObject {
    
    public JSONXObject() {
    }

    @Override
    public JsonValue.ValueType getValueType() {
        return JsonValue.ValueType.OBJECT;
    }

    @Override
    public boolean getBoolean(String name) {
        JsonValue val = get(name);
        return val == javax.json.JsonValue.TRUE;
    }

    @Override
    public boolean getBoolean(String name, boolean bln) {
        JsonValue val = get(name);
        return val == null ? bln : val == JsonValue.TRUE;
    }

    @Override
    public int getInt(String name) {
        return getJsonNumber(name).intValue();
    }

    @Override
    public boolean isNull(String name) {
        return get(name) == JsonValue.NULL;
    }

    @Override
    public JsonArray getJsonArray(String name) {
        return (JsonArray) get(name);
    }

    @Override
    public JsonObject getJsonObject(String name) {
        return (JsonObject) get(name);
    }

    @Override
    public JsonNumber getJsonNumber(String name) {
        return (JsonNumber) get(name);
    }

    @Override
    public JsonString getJsonString(String name) {
        return (JsonString) get(name);
    }

    @Override
    public String getString(String name) {
        return getJsonString(name).getString();
    }

    @Override
    public String getString(String name, String string1) {
        JsonString val = getJsonString(name);
        return val == null ? string1 : val.getString();
    }

    @Override
    public int getInt(String name, int i) {
        JsonNumber val = getJsonNumber(name);
        return val == null ? i : val.intValue();
    }

    @Override
    public JsonValue getValue(String jsonPointer) {
        return new JSONPointer(jsonPointer).step((JsonValue)this,false).val;
    }
}
