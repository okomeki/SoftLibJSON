package net.siisise.json.map;

import java.util.Map;
import net.siisise.json.JSONObject;
import net.siisise.json.JSONReplaceOM;
import net.siisise.json.JSONReplacer;

/**
 *
 */
public class JSONObjectM implements JSONReplaceOM {
    
    /**
     *
     * @param src
     * @param replacer
     * @return
     */
    @Override
    public JSONObject valueOf(Object src, JSONReplacer replacer) {
        if (src instanceof Map) {
            if ( replacer != null ) {
                Map m = (Map)src;
                for ( Object key : m.keySet()) {
                    Object val = m.get(key);
                    val = replacer.replacer((String)key, val);
                    m.put(key, val);
                }
                src = m;
            }
            return new JSONObject((Map) src);
        }
        return null;
    }

}
