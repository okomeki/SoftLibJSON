package net.siisise.json.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFSelect;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONtextParser extends ABNFSelect<JSONValue> {

    public JSONtextParser(ABNF rule, ABNFReg base) {
        super(rule, base, "value");
    }

}
