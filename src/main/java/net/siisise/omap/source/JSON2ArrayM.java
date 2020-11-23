package net.siisise.omap.source;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import net.siisise.json2.JSON2Array;
import net.siisise.omap.MtoConvert;
import net.siisise.omap.OMConvert;

/**
 *
 */
public class JSON2ArrayM implements OMConvert {

    @Override
    public Class[] getSrcClasses() {
        return new Class[] { Collection.class, ArrayList.class };
    }

    @Override
    public Object valueOf(Object src, MtoConvert outConvert) {
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
/*
            if ( replacer != null ) {
                List n = new JSON2Array();
                for ( Object o : (Collection)src) {
                    o = replacer.replacer(null, o);
                    n.add(o);
                }
                src = n;
            }
*/
            return outConvert.listValue((Collection) src);
        }
        return this;
    }
}
