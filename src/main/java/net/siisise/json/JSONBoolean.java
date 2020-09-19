package net.siisise.json;

import javax.json.JsonValue;

/**
 *
 */
public class JSONBoolean extends JSONValue<Boolean> implements JsonValue {

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
     * @param cls
     * @return
     */
    @Override
    public <T> T map(Class<T> cls) {
//        if ( Boolean.TYPE == cls ) {
//            return (T)value;
//        }
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
