package net.siisise.json.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFBaseParser;
import net.siisise.io.Packet;
import net.siisise.json.JSON8259Reg;
import net.siisise.json.JSONMember;
import net.siisise.json.JSONString;
import net.siisise.json.JSONValue;

/**
 *
 * @author okome
 */
public class JSONMemberP extends ABNFBaseParser<JSONMember,JSONValue> {

    public JSONMemberP(ABNFReg reg) {
        super(JSON8259Reg.member, reg, JSONStringP.class, JSONValueP.class);
    }

    @Override
    public JSONMember parse(Packet pac) {
        inst();
        ABNF.C<JSONValue> ret = def.findx(pac, subs);
        if ( ret == null ) return null;
        JSONString str = (JSONString)ret.get(JSON8259Reg.string).get(0);
        JSONValue val = ret.get(JSON8259Reg.value).get(0);
        return new JSONMember(str,val);
    }
    
}
