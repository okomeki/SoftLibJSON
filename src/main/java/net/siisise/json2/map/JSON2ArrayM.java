package net.siisise.json2.map;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import net.siisise.json.JSONReplacer;
import net.siisise.json2.JSON2Array;
import net.siisise.json2.JSON2ReplaceOM;

/**
 *
 */
public class JSON2ArrayM implements JSON2ReplaceOM {

    @Override
    public Object value2Of(Object src, JSONReplacer replacer) {
        if ( src.getClass().isArray() ) {
            Class ar = src.getClass().getComponentType();
            if (ar.isPrimitive()) {
                List cnv;
                int len = Array.getLength(src);
                cnv = new JSON2Array();
                for ( int i = 0; i < len; i++ ) {
                    cnv.add(Array.get(src, i));
                }
                src = cnv;
            } else {
                src = Arrays.asList((Object[]) src);
            }
        }
        if (src instanceof Collection) {
            if ( replacer != null ) {
                List n = new JSON2Array();
                for ( Object o : (Collection)src) {
                    o = replacer.replacer(null, o);
                    n.add(o);
                }
                return n;
            }
            return new JSON2Array((Collection)src);
        }
        return this;
    }
    
}
