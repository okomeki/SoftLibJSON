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
}
