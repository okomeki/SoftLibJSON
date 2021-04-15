package net.siisise.json.parser;

import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;
import net.siisise.json.JSONMember;
import net.siisise.json.JSONString;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONMemberP extends ABNFList<JSONMember, JSONValue> {

    public JSONMemberP(ABNF rule, ABNFReg reg, ABNFReg base) {
        super(rule, reg, base, "string", "value");
    }

    @Override
    protected JSONMember parse(List<JSONValue> list) {
        JSONString str = (JSONString) list.get(0);
        JSONValue val = list.get(1);
        return new JSONMember(str, val);
    }

}
