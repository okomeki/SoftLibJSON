package net.siisise.json.bind.target;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.Map;
import javax.json.JsonValue;

/**
 *
 */
public class JsonValueTypeConvert extends OBJConvert<JsonValue.ValueType> {

    @Override
    public Type targetClass() {
        return JsonValue.ValueType.class;
    }

    @Override
    public JsonValue.ValueType nullValue() {
        return JsonValue.ValueType.NULL;
    }

    @Override
    public JsonValue.ValueType booleanValue(Boolean bool) {
        return bool ? JsonValue.ValueType.TRUE : JsonValue.ValueType.FALSE;
    }

    @Override
    public JsonValue.ValueType numberValue(Number num) {
        return JsonValue.ValueType.NUMBER;
    }

    @Override
    public JsonValue.ValueType stringValue(CharSequence str) {
        return JsonValue.ValueType.STRING;
    }

    @Override
    public JsonValue.ValueType listValue(Collection list) {
        return JsonValue.ValueType.ARRAY;
    }

    @Override
    public JsonValue.ValueType mapValue(Map map) {
        return JsonValue.ValueType.OBJECT;
    }
    
}
