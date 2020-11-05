package net.siisise.json2;

import java.lang.reflect.Type;
import javax.json.JsonValue;
import net.siisise.json.JSONFormat;
import net.siisise.json.JSONNULL;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSON2NULL implements JSON2Value {
    
    public static final JSON2NULL NULL = new JSON2NULL();

    @Override
    public <T> T map() {
        return null;
    }

    @Override
    public Object typeMap(Type type) {
        if ( type == JSONValue.class || type == JSONNULL.class ) {
            return JSONNULL.NULL;
        } else if ( type == JSON2Value.class || type == JSON2NULL.class ) {
            return JSON2NULL.NULL;
        } else if ( type == JsonValue.class ) {
            return JsonValue.NULL;
        }
        return null;
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
    public String toString(JSONFormat format) {
        return "null";
    }
    
}
