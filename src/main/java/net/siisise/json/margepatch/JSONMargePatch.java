package net.siisise.json.margepatch;

import net.siisise.json.JSONNULL;
import net.siisise.json.JSONObject;
import net.siisise.json.JSONValue;

/**
 * RFC 7396 JSON Merge Patch
 * @see https://tools.ietf.org/html/rfc7396
 */
public class JSONMargePatch {
    public static JSONValue mergePatch(JSONValue target, JSONValue patch) {
        if ( patch instanceof JSONObject ) {
            if ( !(target instanceof JSONObject) ) {
                target = new JSONObject();
            }
            for ( String name : ((JSONObject) patch).keySet() ) {
                JSONValue value = ((JSONObject) patch).get(name);
                if ( value instanceof JSONNULL ) {
                    if ( ((JSONObject)target).get(name) != null ) {
                        ((JSONObject)target).remove(name);
                    }
                } else {
                    ((JSONObject)target).add(name, mergePatch(((JSONObject)target).get(name), value));
                }
            }
            return target;
        } else {
            return patch;
        }
    }
}
