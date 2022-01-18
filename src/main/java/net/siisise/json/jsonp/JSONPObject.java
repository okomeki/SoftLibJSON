package net.siisise.json.jsonp;

import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import net.siisise.json.JSONPointer;
import net.siisise.json.JSON2Object;

/**
 * それっぽく互換性をかもしだしたもの
 */
public class JSONPObject extends JSON2Object<JsonValue> implements JsonObject {
    
    public JSONPObject() {
    }

    @Override
    public ValueType getValueType() {
        return ValueType.OBJECT;
    }

    @Override
    public boolean getBoolean(String name) {
        JsonValue val = get(name);
        return val == JsonValue.TRUE;
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
