package net.siisise.omap.source;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import net.siisise.io.BASE64;
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
        Class cls = src.getClass();
        if ( cls.isArray() ) {
            Class ar = cls.getComponentType();
            if (ar.isPrimitive()) {
                List cnv;
                int len = Array.getLength(src);
                // byte[], char[] は別にするかも
                if ( ar == Byte.TYPE ) {
                    BASE64 b64 = new BASE64(BASE64.URL,0);
                    String b64src = b64.encode((byte[])src);
                    
                } else if ( ar == Character.TYPE ) {
                    
                }
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
