package net.siisise.json.map;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONReplaceOM;
import net.siisise.json.JSONReplacer;

/**
 *
 */
public class JSONArrayM implements JSONReplaceOM {

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
}
