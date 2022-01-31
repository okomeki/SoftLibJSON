package net.siisise.json.parser;

import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.bnf.parser.BNFList;
import net.siisise.json.JSON2Member;
import net.siisise.json.JSON2Object;

/**
 * JSON Objectの組み立て
 */
public class JSONObjectP extends BNFList<JSON2Object, JSON2Member> {

    public JSONObjectP(ABNF rule, ABNFReg base) {
        super(rule, base, "member");
    }

    @Override
    protected JSON2Object build(List<JSON2Member> mlist) {
        JSON2Object obj = new JSON2Object();
        if (mlist != null) {
            mlist.forEach(mem -> {
                obj.put(mem.key, mem.value);
            });
        }
        return obj;
    }

}
