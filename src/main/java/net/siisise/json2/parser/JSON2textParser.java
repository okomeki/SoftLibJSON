package net.siisise.json2.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFSub;

/**
 *
 */
public class JSON2textParser extends ABNFSub<Object> {
    
    public JSON2textParser(ABNF rule, ABNFReg reg, ABNFReg base) {
        super(rule, reg, base, "value");
    }
}
