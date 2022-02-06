package net.siisise.json.parser;

import java.util.List;
import net.siisise.bnf.BNF;
import net.siisise.bnf.BNFReg;
import net.siisise.bnf.parser.BNFList;
import net.siisise.json.JSONMember;

/**
 * object内のmember. stringとvalueの仮の組
 */
public class JSONMemberP extends BNFList<JSONMember, Object> {

    public JSONMemberP(BNF rule, BNFReg base) {
        super(rule, base, "string", "value");
    }

    @Override
    protected JSONMember build(List<Object> val) {
        String str = (String) val.get(0);
        Object value = val.get(1);
        return new JSONMember(str, value);
    }

}
