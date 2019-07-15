package net.siisise.json.parser;

import java.util.List;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;
import net.siisise.json.JSON8259Reg;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONValue;

/**
 *
 * @author okome
 */
public class JSONArrayP extends ABNFList<JSONArray, JSONValue> {

    public JSONArrayP(ABNFReg reg) {
        super(JSON8259Reg.array, reg, JSONValueP.class);
    }

    @Override
    public JSONArray parse(List<JSONValue> val) {
        return new JSONArray(val);
    }

}
