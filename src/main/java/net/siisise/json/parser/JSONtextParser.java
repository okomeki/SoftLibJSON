package net.siisise.json.parser;

import net.siisise.bnf.BNF;
import net.siisise.bnf.BNFReg;
import net.siisise.bnf.parser.BNFSelect;

/**
 * JSON-textのParser.
 * JSON-text = vs value vs なので、中からBNFSelectでvalueを抽出するだけ
 */
public class JSONtextParser extends BNFSelect<Object> {
    
    public JSONtextParser(BNF rule, BNFReg base) {
        super(rule, base, "value");
    }
}
