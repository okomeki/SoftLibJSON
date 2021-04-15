package net.siisise.json2.parser;

import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;
import net.siisise.json2.JSON2Member;

/**
 *
 */
public class JSON2MemberP extends ABNFList<JSON2Member, Object> {

    public JSON2MemberP(ABNF rule, ABNFReg reg, ABNFReg base) {
        super(rule, reg, base, "string", "value");
    }

    @Override
    protected JSON2Member parse(List<Object> val) {
        String str = (String) val.get(0);
        Object value = val.get(1);
        return new JSON2Member(str, value);
    }

}
