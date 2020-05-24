package net.siisise.json.map;

import javax.json.JsonNumber;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONReplaceOM;
import net.siisise.json.JSONReplacer;

public class JSONNumberM implements JSONReplaceOM {

    @Override
    public JSONNumber valueOf(Object obj, JSONReplacer r) {
        if ( obj instanceof Number ) {
            return new JSONNumber((Number)obj);
        } else if ( obj instanceof JsonNumber ) {
            return new JSONNumber(((JsonNumber) obj).numberValue());
        }
        return null;
    }    
}
