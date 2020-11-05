package net.siisise.json2;

import java.lang.reflect.Type;
import javax.json.JsonValue;
import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONFormat;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSON2Boolean implements JSON2Value {
    
    public static final JSON2Boolean TRUE = new JSON2Boolean(true);
    public static final JSON2Boolean FALSE = new JSON2Boolean(false);

    private boolean bool;
    
    JSON2Boolean(boolean b) {
        bool = b;
    }

    @Override
    public <T> T typeMap(Type type) {
        if ( type instanceof Class ) {
            Class<T> cls = (Class<T>) type;
            if ( cls == Boolean.class || cls == Boolean.TYPE ) {
                return (T)(Boolean)bool;
            } else if ( type == JsonValue.class ) {
                return (T)(bool ? JsonValue.TRUE : JsonValue.FALSE);
            } else if ( type == JSONBoolean.class || type == JSONValue.class ) {
                return (T)(bool ? JSONBoolean.TRUE : JSONBoolean.FALSE);
            } else if ( type == JSON2Boolean.class || type == JSON2Value.class ) {
                return (T)(bool ? JSON2Boolean.TRUE : JSON2Boolean.FALSE);
            } else if ( cls.isAssignableFrom(Integer.class) ) {
                return (T)Integer.valueOf( bool ? 1 : 0 );
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
        return toString(TAB);
    }

    @Override
    public String toString(JSONFormat format) {
        return Boolean.toString(bool);
    }

    @Override
    public <T> T map() {
        return (T)Boolean.valueOf(bool);
    }
}
