package net.siisise.json.map;

import javax.json.JsonNumber;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONReplaceOM;
import net.siisise.json.JSONReplacer;
import net.siisise.json2.JSON2Number;

public class JSONNumberM implements JSONReplaceOM {

    @Override
    public JSONNumber valueOf(Object obj, JSONReplacer r) {
        if ( obj instanceof JSON2Number ) { // Number 互換なので先
            return new JSONNumber(((JSON2Number)obj).map());
        } else if ( obj instanceof Number ) {
            return new JSONNumber((Number)obj);
        } else if ( obj instanceof JsonNumber ) {
            return new JSONNumber(((JsonNumber) obj).numberValue());
        }
        return null;
    }    
}
