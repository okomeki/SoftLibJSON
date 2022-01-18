package net.siisise.json.parser;

import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;
import net.siisise.json.JSON2Member;

/**
 * object内のmember. stringとvalueの仮の組
 */
public class JSON2MemberP extends ABNFList<JSON2Member, Object> {

    public JSON2MemberP(ABNF rule, ABNFReg base) {
        super(rule, base, "string", "value");
    }

    @Override
    protected JSON2Member build(List<Object> val) {
        String str = (String) val.get(0);
        Object value = val.get(1);
        return new JSON2Member(str, value);
    }

}
