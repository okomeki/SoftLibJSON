package net.siisise.json.bind.target;

import java.util.Collection;
import java.util.Map;
import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonValue;
import net.siisise.json.jsonp.JSONPArray;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONObject;
import net.siisise.json.JSONString;
import net.siisise.json.bind.OMAP;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JsonxpConvert extends OBJConvert<JsonValue> {
    
    @Override
    public Class<JsonValue> targetClass() {
        return JsonValue.class;
    }

    @Override
    public JsonValue nullValue() {
        return JsonValue.NULL;
    }

    @Override
    public JsonValue booleanValue(Boolean bool) {
        return bool ? JsonValue.TRUE : JsonValue.FALSE;
    }

    @Override
    public JsonNumber numberValue(Number number) {
        return new JSONNumber(number);
    }

    @Override
    public JsonValue stringValue(CharSequence str) {
        return new JSONString(str);
    }

    @Override
    public JsonArray listValue(Collection list) {
        if ( list.isEmpty() ) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }
        return (JsonArray) list.stream().collect(JSONPArray.collector());
    }

    @Override
    public JsonObject mapValue(Map map) {
        if ( map.isEmpty() ) {
            return JsonValue.EMPTY_JSON_OBJECT;
        }
        return (JsonObject) new JSONObject(map).toJson();
    }

    /**
     * 表面のみ変換
     * @param obj
     * @return 
     */
    @Override
    public JsonValue objectValue(Object obj) {
        // toJSON メソッドで変換
        JSONValue json = OMAP.toJSON(obj);
        if ( json != null ) {
            return json.toXJson();
        }
        return super.objectValue(obj);
    }
}