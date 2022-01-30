package net.siisise.json.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.bnf.parser.BNFSelect;
import net.siisise.io.FrontPacket;

public class JSON2ValueP extends BNFSelect<Object> {
    
    public JSON2ValueP(ABNF rule, ABNFReg base) {
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
