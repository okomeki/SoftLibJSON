package net.siisise.json.parser;

import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFSub;
import net.siisise.json.JSON8259Reg;
import net.siisise.json.JSONValue;

/**
 *
 * @author okome
 */
public class JSONtextParser extends ABNFSub<JSONValue> {

    public JSONtextParser(ABNFReg reg) {
        super(JSON8259Reg.JSONtext, reg, JSONValueP.class);
    }

}
