package net.siisise.json;

import java.lang.reflect.Type;
import javax.json.JsonValue;

/**
 *
 */
public class JSONBoolean extends JSONValue<Boolean> implements JsonValue {
    
    public static final JSONBoolean TRUE = new JSONBoolean(true);
    public static final JSONBoolean FALSE = new JSONBoolean(false);

    public JSONBoolean(Boolean b) {
        value = b;
    }

    @Override
    public Boolean map() {
        return value;
    }

    /**
     * boolean,Boolean,String ぐらいに対応
     * 
     * @param <T>
     * @param type
     * @return 
     */
    @Override
    public <T> T typeMap(Type type) {
        return (T) value;
    }
    
    @Override
    public ValueType getValueType() {
        return value ? JsonValue.ValueType.TRUE : JsonValue.ValueType.FALSE;
    }
    
    @Override
    public JsonValue toJson() {
        return value ? JsonValue.TRUE : JsonValue.FALSE;
    }

}
