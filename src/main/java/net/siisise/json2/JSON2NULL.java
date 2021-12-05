package net.siisise.json2;

import java.lang.reflect.Type;
import javax.json.JsonValue;
import net.siisise.omap.OMAP;

/**
 *
 */
public class JSON2NULL implements JSON2Value,JsonValue {
    
    public static final JSON2NULL NULL = new JSON2NULL();

    @Override
    public <T> T map() {
        return null;
    }

    @Override
    public Object typeMap(Type type) {
        return OMAP.typeNull(type);
    }

    @Override
    public JsonValue toJson() {
        return JsonValue.NULL;
    }
    
    @Override
    public String toString() {
        return toString(TAB);
    }

    @Override
    public String toString(JSON2Format format) {
        return "null";
    }

    @Override
    public ValueType getValueType() {
        return ValueType.NULL;
    }
    
}
