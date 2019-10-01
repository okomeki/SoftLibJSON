package net.siisise.json.parser;

import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONArrayP extends ABNFList<JSONArray, JSONValue> {

    public JSONArrayP(ABNF def, ABNFReg reg, ABNFReg base) {
        super(def, reg, base, "value");
    }

    @Override
    public JSONArray parse(List<JSONValue> val) {
        return new JSONArray(val);
    }

}
