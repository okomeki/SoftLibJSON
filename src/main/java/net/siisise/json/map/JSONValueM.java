package net.siisise.json.map;

import javax.json.JsonValue;
import net.siisise.json.JSONReplaceOM;
import net.siisise.json.JSONReplacer;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONValueM implements JSONReplaceOM {

    @Override
    public JSONValue valueOf(Object obj, JSONReplacer r) {
        if ( obj == null || obj == JsonValue.NULL ) {
            return JSONValue.NULL; //new JSONNULL();
        } else if (obj instanceof JSONValue) {
            return (JSONValue) obj; // ToDo: 複製
        } else if ( obj instanceof Boolean ) {
            return ((Boolean)obj) ? JSONValue.TRUE : JSONValue.FALSE; // new JSONBoolean((Boolean)obj);
        } else if ( obj == JsonValue.TRUE ) {
            return JSONValue.TRUE;
        } else if ( obj == JsonValue.FALSE ) {
            return JSONValue.FALSE;
        }
        return null;
    }
}
