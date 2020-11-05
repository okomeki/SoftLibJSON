package net.siisise.json2.map;

import javax.json.JsonValue;
import net.siisise.json.JSONReplacer;
import net.siisise.json.JSONValue;
import net.siisise.json2.JSON2ReplaceOM;

/**
 * Boolean, NULL, JSONValue
 */
public class JSON2ValueM implements JSON2ReplaceOM {

    @Override
    public Object value2Of(Object obj, JSONReplacer replacer) {
        if ( obj == null || obj == JsonValue.NULL ) {
            return null;
        } else if (obj instanceof JSONValue ) {
            return ((JSONValue)obj).map();
        } else if ( obj instanceof Boolean ) {
            return obj;
        } else if (obj == JsonValue.TRUE ) {
            return Boolean.TRUE;
        } else if (obj == JsonValue.FALSE ) {
            return Boolean.FALSE;
        }

        return this;
    }
    
}
