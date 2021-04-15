package net.siisise.json.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFSub;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONtextParser extends ABNFSub<JSONValue> {

    public JSONtextParser(ABNF rule, ABNFReg reg, ABNFReg base) {
        super(rule, reg, base, "value");
    }

}
