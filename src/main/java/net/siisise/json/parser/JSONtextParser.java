package net.siisise.json.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFSub;
import net.siisise.json.JSONValue;

/**
 *
 * @author okome
 */
public class JSONtextParser extends ABNFSub<JSONValue> {

    public JSONtextParser(ABNF def, ABNFReg reg, ABNFReg base) {
        super(def, reg, base, "value");
    }

}
