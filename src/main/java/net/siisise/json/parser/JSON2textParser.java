package net.siisise.json.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFSelect;

/**
 *
 */
public class JSON2textParser extends ABNFSelect<Object> {
    
    public JSON2textParser(ABNF rule, ABNFReg base) {
        super(rule, base, "value");
    }
}
