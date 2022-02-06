package net.siisise.json.parser;

import net.siisise.bnf.BNF;
import net.siisise.bnf.BNFReg;
import net.siisise.bnf.parser.BNFSelect;

/**
 *
 */
public class JSONtextParser extends BNFSelect<Object> {
    
    public JSONtextParser(BNF rule, BNFReg base) {
        super(rule, base, "value");
    }
}
