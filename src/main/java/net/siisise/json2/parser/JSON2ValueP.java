package net.siisise.json2.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFSelect;
import net.siisise.io.FrontPacket;
import net.siisise.json2.JSON28259Reg;

public class JSON2ValueP extends ABNFSelect<Object> {
    
    public JSON2ValueP(ABNF rule, ABNFReg base) {
        super(rule, base, "object", "array", "number", "string");
    }
    
    @Override
    protected Object other(FrontPacket pac) {
        FrontPacket p = JSON28259Reg.FALSE.is(pac);
        if (p != null) {
            return Boolean.FALSE;
        }
        p = JSON28259Reg.NULL.is(pac);
        if (p != null) {
            return null;
        }
        p = JSON28259Reg.TRUE.is(pac);
        if (p != null) {
            return Boolean.TRUE;
        }
        return null;
    }
}
