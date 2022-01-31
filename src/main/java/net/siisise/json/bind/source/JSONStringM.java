package net.siisise.json.bind.source;

import javax.json.JsonString;
import net.siisise.json.JSON2String;
import net.siisise.json.bind.MtoConvert;
import net.siisise.json.bind.OMConvert;

/**
 *
 */
public class JSONStringM implements OMConvert {

    @Override
    public Class[] getSrcClasses() {
        return new Class[] { JsonString.class, JSON2String.class, String.class, CharSequence.class };
    }

    @Override
    public Object valueOf(Object obj, MtoConvert outConvert) {
        if ( obj instanceof JsonString ) { // JSON2String も該当
            obj = ((JsonString)obj).getString();
//        } else if ( obj instanceof UUID ) {
//            obj = ((UUID)obj).toString();
        }
        if ( obj instanceof String ) {
            return outConvert.stringValue((String)obj);
        }
        if ( obj instanceof CharSequence ) {
            return outConvert.stringValue((CharSequence)obj);
        }
        return this;
    }
    
}
