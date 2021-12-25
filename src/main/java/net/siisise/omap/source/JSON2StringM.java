package net.siisise.omap.source;

import javax.json.JsonString;
import net.siisise.json2.JSON2String;
import net.siisise.omap.MtoConvert;
import net.siisise.omap.OMConvert;

/**
 *
 */
public class JSON2StringM implements OMConvert {

    @Override
    public Class[] getSrcClasses() {
        return new Class[] { JsonString.class, JSON2String.class, String.class, CharSequence.class };
    }

    @Override
    public Object valueOf(Object obj, MtoConvert outConvert) {
        if ( obj instanceof JsonString ) { // JSONString. JSON2String も該当
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
