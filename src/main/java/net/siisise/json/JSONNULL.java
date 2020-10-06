package net.siisise.json;

import javax.json.JsonValue;

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
    public Object map(Class cls) {
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
