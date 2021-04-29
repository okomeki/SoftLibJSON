package net.siisise.json.parser;

import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;
import net.siisise.json.JSONMember;
import net.siisise.json.JSONObject;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONObjectP extends ABNFList<JSONValue, JSONMember> {

    public JSONObjectP(ABNF rule, ABNFReg base) {
        super(rule, base, "member");
    }

    @Override
    public JSONObject build(List<JSONMember> mlist) {
        JSONObject obj = new JSONObject();
        if ( mlist != null ) {
            mlist.forEach(mem -> {
                obj.setJSON(mem.str, mem.value);
            });
        }
        return obj;
    }
}
