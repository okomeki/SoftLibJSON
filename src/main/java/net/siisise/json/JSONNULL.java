package net.siisise.json;

import java.lang.reflect.Type;
import javax.json.JsonValue;
import net.siisise.omap.OMAP;

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
        return OMAP.typeNull(type);
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
