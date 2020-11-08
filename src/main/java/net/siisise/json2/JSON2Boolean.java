package net.siisise.json2;

import java.lang.reflect.Type;
import javax.json.JsonValue;
import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONFormat;

/**
 *
 */
public class JSON2Boolean implements JSON2Value,JsonValue {
    
    public static final JSON2Boolean TRUE = new JSON2Boolean(true);
    public static final JSON2Boolean FALSE = new JSON2Boolean(false);

    private final boolean bool;
    
    public JSON2Boolean(boolean b) {
        bool = b;
    }

    @Override
    public <T> T typeMap(Type type) {
        if ( type instanceof Class ) {
            Class<T> cls = (Class<T>) type;
            if ( cls.isAssignableFrom(Boolean.class) || cls == Boolean.TYPE ) {
                return (T)(Boolean)bool;
            } else if ( cls == String.class || cls == CharSequence.class ) {
                return (T)toString();
            } else if ( type == JsonValue.class ) {
                return (T)toJson();
            } else if ( cls.isAssignableFrom(JSON2Boolean.class) ) {
                return (T)(bool ? JSON2Boolean.TRUE : JSON2Boolean.FALSE);
            } else if ( cls.isAssignableFrom(JSONBoolean.class) ) {
                return (T)(bool ? JSONBoolean.TRUE : JSONBoolean.FALSE);
            } else if ( cls.isAssignableFrom(Integer.class) ) {
                return (T)Integer.valueOf( bool ? 1 : 0 );
            } else if ( cls.isAssignableFrom(Byte.class) ) {
                return (T)Byte.valueOf( bool ? (byte)1 : (byte)0 );
            } else if ( cls.isAssignableFrom(Short.class) ) {
                return (T)Short.valueOf( bool ? (short)1 : (short)0 );
            } else if ( cls.isAssignableFrom(Long.class) ) {
                return (T)Long.valueOf( bool ? 1 : 0 );
            } else if ( type == JsonValue.ValueType.class ) {
                return (T)getValueType();
            }
        }
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public JsonValue toJson() {
        return bool ? JsonValue.TRUE : JsonValue.FALSE;
    }
    
    @Override
    public String toString() {
        return toString(NOBR);
    }
    
    @Override
    public String toString(JSONFormat format) {
        return Boolean.toString(bool);
    }

    @Override
    public <T> T map() {
        return (T)Boolean.valueOf(bool);
    }

    @Override
    public ValueType getValueType() {
        return bool ? JsonValue.ValueType.TRUE : JsonValue.ValueType.FALSE;
    }
}
