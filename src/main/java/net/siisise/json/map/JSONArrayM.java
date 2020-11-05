package net.siisise.json.map;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONReplaceOM;
import net.siisise.json.JSONReplacer;
import net.siisise.json2.JSON2Array;
import net.siisise.json2.JSON2ReplaceOM;

/**
 *
 */
public class JSONArrayM implements JSONReplaceOM,JSON2ReplaceOM {

    /**
     *
     * @param src
     * @param replacer
     * @return
     */
    @Override
    public JSONArray valueOf(Object src, JSONReplacer replacer) {
        if ( src.getClass().isArray()) {
            Class ar = src.getClass().getComponentType();
            if (ar.isPrimitive()) {
                List cnv;
                int len = Array.getLength(src);
                cnv = new ArrayList();
                for (int i = 0; i < len; i++) {
                    cnv.add(Array.get(src, i));
                }
                src = cnv;
            } else {
                src = Arrays.asList((Object[]) src);
            }
        }
        if (src instanceof Collection) {
            if ( replacer != null ) {
                List n = new ArrayList();
                for ( Object o : (Collection)src) {
                    o = replacer.replacer(null, o);
                    n.add(o);
                }
                src = n;
            }
            return new JSONArray((Collection) src);
        }
        return null;
    }

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
