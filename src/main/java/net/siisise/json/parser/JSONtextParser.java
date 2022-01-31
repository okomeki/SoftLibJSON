package net.siisise.json.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.bnf.parser.BNFSelect;

/**
 *
 */
public class JSONtextParser extends BNFSelect<Object> {
    
    public JSONtextParser(ABNF rule, ABNFReg base) {
        super(rule, base, "value");
    }
}
