package net.siisise.json2.map;

import javax.json.JsonString;
import net.siisise.json.JSONReplacer;
import net.siisise.json2.JSON2ReplaceOM;

/**
 *
 */
public class JSON2StringM implements JSON2ReplaceOM {

    @Override
    public Object value2Of(Object obj, JSONReplacer replacer) {
        if ( obj instanceof String ) {
            return obj;
        } else if ( obj instanceof JsonString ) {
            return ((JsonString)obj).getString();
        }
        return this;
    }
    
}
