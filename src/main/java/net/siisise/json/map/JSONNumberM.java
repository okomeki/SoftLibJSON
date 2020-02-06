package net.siisise.json.map;

import net.siisise.json.JSONNumber;
import net.siisise.json.JSONReplaceOM;
import net.siisise.json.JSONReplacer;

public class JSONNumberM implements JSONReplaceOM {

    @Override
    public JSONNumber valueOf(Object obj, JSONReplacer r) {
        if ( obj instanceof Number ) {
            return new JSONNumber((Number)obj);
        }
        return null;
    }    
}
