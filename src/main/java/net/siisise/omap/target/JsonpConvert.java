package net.siisise.omap.target;

import java.util.Collection;
import java.util.Map;
import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonValue;
import net.siisise.json2.jsonp.JSONPArray;
import net.siisise.json2.JSON2;
import net.siisise.json2.JSON2Number;
import net.siisise.json2.JSON2Object;
import net.siisise.json2.JSON2String;
import net.siisise.omap.OMAP;

/**
 *
 */
public class JsonpConvert extends OBJConvert<JsonValue> {

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
        return new JSON2Number(number);
    }

    @Override
    public JsonValue stringValue(CharSequence str) {
        return new JSON2String(str);
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
        return (JsonObject) new JSON2Object(map).toJson();
    }

    /**
     * 表面のみ変換
     * @param obj
     * @return 
     */
    @Override
    public JsonValue objectValue(Object obj) {
        // toJSON メソッドで変換
        String json = OMAP.toJSON(obj);
        if ( json != null ) {
            return JSON2.parseWrap(json).toJson();
        }
        return super.objectValue(obj);
    }
}
