package net.siisise.json.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.bnf.parser.BNFSelect;

/**
 *
 */
public class JSON2textParser extends BNFSelect<Object> {
    
    public JSON2textParser(ABNF rule, ABNFReg base) {
        super(rule, base, "value");
    }
}
