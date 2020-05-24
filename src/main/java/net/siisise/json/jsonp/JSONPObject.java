package net.siisise.json.jsonp;

import java.util.HashMap;
import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import net.siisise.json.pointer.JSONPointer;

/**
 *
 */
public class JSONPObject extends HashMap<String,JsonValue> implements JsonObject {

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
        return new JSONPointer(jsonPointer).step(this,false).val;
    }

    @Override
    public JsonObject asJsonObject() {
        return this;
    }

    @Override
    public JsonArray asJsonArray() {
        return JsonObject.super.asJsonArray(); //To change body of generated methods, choose Tools | Templates.
    }
    
}
