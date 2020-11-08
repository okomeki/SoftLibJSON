package net.siisise.json;

import java.lang.reflect.Type;
import javax.json.JsonValue;
import net.siisise.json2.JSON2Boolean;

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
        if ( type instanceof Class ) {
            Class cls = (Class)type;
            if (cls == String.class || cls == CharSequence.class) {
                return (T)toString();
            } else if (cls.isAssignableFrom(Boolean.class) || cls == Boolean.TYPE) {
                return (T)value;
            } else if ( cls == JsonValue.class ) {
                return (T)toJson();
            } else if ( cls.isAssignableFrom(JSON2Boolean.class) ) {
                return (T)(value ? JSON2Boolean.TRUE : JSON2Boolean.FALSE);
            } else if ( cls.isAssignableFrom( JSONBoolean.class) ) {
                return (T)(value ? JSONBoolean.TRUE : JSONBoolean.FALSE);
            } else if ( cls.isAssignableFrom(Integer.class) ) {
                return (T)Integer.valueOf( value ? 1 : 0 );
            } else if ( cls.isAssignableFrom(Long.class) ) {
                return (T)Long.valueOf( value ? (long)1 : (long)0 );
            } else if ( cls.isAssignableFrom(Short.class) ) {
                return (T)Short.valueOf( value ? (short)1 : (short)0 );
            } else if ( cls.isAssignableFrom(Byte.class) ) {
                return (T)Byte.valueOf( value ? (byte)1 : (byte)0 );
            } else if ( cls == JsonValue.ValueType.class ) {
                return (T)getValueType();
            }
        }
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
