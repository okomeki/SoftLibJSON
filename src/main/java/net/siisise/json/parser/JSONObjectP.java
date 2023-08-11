package net.siisise.json.parser;

import java.util.List;
import net.siisise.bnf.BNF;
import net.siisise.bnf.BNFReg;
import net.siisise.bnf.parser.BNFList;
import net.siisise.json.JSONMember;
import net.siisise.json.JSONObject;

/**
 * JSON Objectの組み立て
 */
public class JSONObjectP extends BNFList<JSONObject, JSONMember> {

    public JSONObjectP(BNF rule, BNFReg base) {
        super(rule, base, "member");
    }

    @Override
    protected JSONObject build(List<JSONMember> mlist) {
        JSONObject obj = new JSONObject();
        if (mlist != null) {
            mlist.forEach(mem -> {
                obj.put(mem.key, mem.value);
            });
        }
//        return (JSONObject) ((UnbindABNFReg)base).getFormat().mapFormat(obj);
        return obj;
    }

}
