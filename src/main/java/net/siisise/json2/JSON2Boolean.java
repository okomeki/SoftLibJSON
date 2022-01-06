package net.siisise.json2;

import java.lang.reflect.Type;
import javax.json.JsonValue;
import net.siisise.omap.OMAP;

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
    
    public static JSON2Boolean valieOf(boolean b) {
        return b ? TRUE : FALSE;
    }

    @Override
    public <T> T typeMap(Type type) {
        return (T)OMAP.typeBoolean(bool, type);
    }

    @Override
    public JsonValue toJson() {
        return bool ? JsonValue.TRUE : JsonValue.FALSE;
    }
    
    @Override
    public String toString() {
        return Boolean.toString(bool);
    }
    
    @Override
    public String toString(JSON2Format format) {
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
    
    @Override
    public boolean equals(Object obj) {
        if ( obj instanceof JSON2Boolean) {
            return bool == (Boolean)((JSON2Boolean) obj).map();
        } else if ( obj == JsonValue.TRUE ) {
            return bool;
        } else if ( obj == JsonValue.FALSE ) {
            return !bool;
        }
        return false;
    }
    
}
