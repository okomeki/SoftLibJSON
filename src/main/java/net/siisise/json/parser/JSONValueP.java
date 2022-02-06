package net.siisise.json.parser;

import net.siisise.bnf.BNF;
import net.siisise.bnf.BNFReg;
import net.siisise.bnf.parser.BNFSelect;
import net.siisise.io.FrontPacket;

public class JSONValueP extends BNFSelect<Object> {
    
    public JSONValueP(BNF rule, BNFReg base) {
        super(rule, base, "object", "array", "number", "string");
    }
    
    @Override
    protected Object other(FrontPacket pac) {
        FrontPacket p = JSON8259Reg.FALSE.is(pac);
        if (p != null) {
            return Boolean.FALSE;
        }
        p = JSON8259Reg.NULL.is(pac);
        if (p != null) {
            return null;
        }
        p = JSON8259Reg.TRUE.is(pac);
        if (p != null) {
            return Boolean.TRUE;
        }
        return null;
    }
}
