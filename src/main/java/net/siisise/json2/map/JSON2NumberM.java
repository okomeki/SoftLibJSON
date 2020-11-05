package net.siisise.json2.map;

import javax.json.JsonNumber;
import net.siisise.json.JSONReplacer;
import net.siisise.json2.JSON2ReplaceOM;

/**
 *
 */
public class JSON2NumberM implements JSON2ReplaceOM {

    @Override
    public Object value2Of(Object obj, JSONReplacer replacer) {
//        if ( obj instanceof JSON2Number ) {
//            obj = ((JSON2Number)obj).map();
//        }
        if ( obj instanceof Number ) {
            return obj;
        } else if ( obj instanceof JsonNumber ) {
            return ((JsonNumber)obj).numberValue();
        }
        return this;
    }
    
}
