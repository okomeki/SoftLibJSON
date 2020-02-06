package net.siisise.json.map;

import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONNULL;
import net.siisise.json.JSONReplaceOM;
import net.siisise.json.JSONReplacer;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONValueM implements JSONReplaceOM {

    @Override
    public JSONValue valueOf(Object obj, JSONReplacer r) {
        if ( obj == null) {
            return new JSONNULL();
        } else if (obj instanceof JSONValue) {
            return (JSONValue) obj;
        } else if ( obj instanceof Boolean ) {
            return new JSONBoolean((Boolean)obj);
        }
        return null;
    }
}
