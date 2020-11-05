package net.siisise.json2.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFBaseParser;
import net.siisise.io.FrontPacket;
import net.siisise.json2.JSON28259Reg;
import net.siisise.json2.JSON2Member;

/**
 *
 */
public class JSON2MemberP extends ABNFBaseParser<JSON2Member, Object> {

    public JSON2MemberP(ABNF def, ABNFReg reg, ABNFReg base) {
        super(def, reg, base, "string", "value");
    }

    @Override
    public JSON2Member parse(FrontPacket pac) {
        inst();
        ABNF.C<Object> ret = def.find(pac, subs);
        if (ret == null) {
            return null;
        }
        String str = (String) ret.get(JSON28259Reg.string).get(0);
        Object val = ret.get(JSON28259Reg.value).get(0);
        return new JSON2Member(str, val);
    }

}
