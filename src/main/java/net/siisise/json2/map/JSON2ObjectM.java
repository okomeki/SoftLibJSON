package net.siisise.json2.map;

import java.util.Map;
import net.siisise.json.JSONReplacer;
import net.siisise.json2.JSON2Object;
import net.siisise.json2.JSON2ReplaceOM;

/**
 *
 */
public class JSON2ObjectM implements JSON2ReplaceOM {

    /**
     *
     * @param src
     * @param replacer
     * @return
     */
    @Override
    public Object value2Of(Object src, JSONReplacer replacer) {
        if (src instanceof Map) {
            if ( replacer != null ) {
                Map m = new JSON2Object();
                Map s = (Map)src;
                s.keySet().forEach(key -> {
                    Object val = s.get(key);
                    val = replacer.replacer((String)key, val);
                    m.put(key, val);
                });
                src = m;
            }
            JSON2Object o = new JSON2Object();
            Map s = (Map)src;
            s.keySet().forEach(key -> {
                Object val = s.get(key);
                o.put(key, val);
            });
            return o;
        }
//        return JSON2Object.convObject(src);
        return this;
    }
    
}
