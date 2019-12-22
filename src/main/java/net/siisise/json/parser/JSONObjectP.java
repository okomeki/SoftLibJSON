package net.siisise.json.parser;

import java.util.List;
import java.util.Map;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;
import net.siisise.json.JSONMember;
import net.siisise.json.JSONObject;
import net.siisise.json.JSONReplacer;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONObjectP extends ABNFList<JSONValue, JSONMember> {

    public JSONObjectP(ABNF def, ABNFReg reg, ABNFReg base) {
        super(def, reg, base, "member");
    }

    @Override
    public JSONObject parse(List<JSONMember> mlist) {
        JSONObject obj = new JSONObject();
        if ( mlist != null ) {
            for (JSONMember mem : mlist) {
                obj.set(mem.str.value(), mem.value);
            }
        }
        return obj;
    }

    /**
     *
     * @param src
     * @param replacer
     * @return
     */
    public static JSONObject valueOf(Object src, JSONReplacer replacer) {
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
