package net.siisise.json.map;

import javax.json.JsonString;
import net.siisise.json.JSONReplaceOM;
import net.siisise.json.JSONReplacer;
import net.siisise.json.JSONString;

public class JSONStringM implements JSONReplaceOM {

    @Override
    public JSONString valueOf(Object obj, JSONReplacer replacer) {
        if ( obj instanceof String ) {
            return new JSONString((String)obj);
        } else if ( obj instanceof JsonString ) {
            return new JSONString(((JsonString)obj).getString());
        }
        return null;
    }
    
}
