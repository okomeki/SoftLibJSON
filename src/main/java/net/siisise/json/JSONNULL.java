package net.siisise.json;

import java.lang.reflect.Type;
import javax.json.JsonValue;
import net.siisise.json2.JSON2NULL;
import net.siisise.json2.JSON2Value;

/**
 * JSONデータ null
 */
public class JSONNULL extends JSONValue implements JsonValue {

    /**
     * NULLの実体はひとつがいい。
     * しんぐるなんとか。
     * Javaのprimitive classの定義は各クラスにあるのでJSONで定義ではなくここ。
     */
    public static final JSONNULL NULL = new JSONNULL();

    public JSONNULL() {

    }

    @Override
    public Object value() {
        return null;
    }

    @Override
    public String toString() {
        return "null";
    }

    @Override
    public String toString(JSONFormat format) {
        return "null";
    }

    @Override
    public Object map() {
        return null;
    }

    @Override
    public Object typeMap(Type type) {
        if ( JSONValue.class.isAssignableFrom((Class)type)) {
            return JSONNULL.NULL;
        }
        if ( JSON2Value.class.isAssignableFrom((Class)type)) {
            return JSON2NULL.NULL;
        }
        if ( type == JsonValue.class ) {
            return JsonValue.NULL;
        }
        return null;
    }
    
    @Override
    public ValueType getValueType() {
        return JsonValue.ValueType.NULL;
    }

    @Override
    public JsonValue toJson() {
        return JsonValue.NULL;
    }
}
