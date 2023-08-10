package net.siisise.json.bind.target;

import java.util.Collection;
import java.util.Map;
import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonValue;
import net.siisise.bind.Rebind;
import net.siisise.bind.format.TypeBind;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONString;
import net.siisise.json.jsonp.JSONPArray;
import net.siisise.json.jsonp.JSONPObject;

/**
 *
 */
public class JsonpConvert implements TypeBind<JsonValue> {

    @Override
    public Class<JsonValue> targetClass() {
        return JsonValue.class;
    }

    @Override
    public JsonValue nullFormat() {
        return JsonValue.NULL;
    }

    @Override
    public JsonValue booleanFormat(boolean bool) {
        return bool ? JsonValue.TRUE : JsonValue.FALSE;
    }

    @Override
    public JsonNumber numberFormat(Number number) {
        return new JSONNumber(number);
    }

    @Override
    public JsonValue stringFormat(String str) {
        return new JSONString(str);
    }

    @Override
    public JsonArray collectionFormat(Collection list) {
        if ( list.isEmpty() ) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }
        return (JsonArray) list.parallelStream().collect(JSONPArray.collector());
    }

    @Override
    public JsonObject mapFormat(Map map) {
        if ( map.isEmpty() ) {
            return JsonValue.EMPTY_JSON_OBJECT;
        } else {
            JSONPObject obj = new JSONPObject();
            for ( Object e : map.entrySet() ) {
                Object k = ((Map.Entry)e).getKey();
                Object v = ((Map.Entry)e).getValue();
                String n = (k instanceof String) ? (String)k : k.toString();
                obj.put(n, Rebind.valueOf(v, this));
            }
            return obj;
        }
    }
}
