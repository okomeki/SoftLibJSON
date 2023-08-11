package net.siisise.json.bind.target;

import java.util.Collection;
import java.util.Map;
import javax.json.JsonValue;
import net.siisise.bind.format.TypeBind;

/**
 * JsonValueのValueType っぽいのにする.
 * 使う?
 */
public class JsonValueTypeConvert implements TypeBind<JsonValue.ValueType> {

    @Override
    public JsonValue.ValueType nullFormat() {
        return JsonValue.ValueType.NULL;
    }

    @Override
    public JsonValue.ValueType booleanFormat(boolean bool) {
        return bool ? JsonValue.ValueType.TRUE : JsonValue.ValueType.FALSE;
    }

    @Override
    public JsonValue.ValueType numberFormat(Number num) {
        return JsonValue.ValueType.NUMBER;
    }

    @Override
    public JsonValue.ValueType stringFormat(String str) {
        return JsonValue.ValueType.STRING;
    }

    @Override
    public JsonValue.ValueType collectionFormat(Collection list) {
        return JsonValue.ValueType.ARRAY;
    }

    @Override
    public JsonValue.ValueType mapFormat(Map map) {
        return JsonValue.ValueType.OBJECT;
    }

}
