package net.siisise.json.parser;

import java.util.List;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;
import net.siisise.json.JSON8259Reg;
import net.siisise.json.JSONMember;
import net.siisise.json.JSONObject;
import net.siisise.json.JSONValue;

/**
 *
 * @author okome
 */
public class JSONObjectP extends ABNFList<JSONValue,JSONMember> {

    public JSONObjectP(ABNFReg reg) {
        super(JSON8259Reg.object, reg, JSONMemberP.class);
    }

    @Override
    public JSONObject parse(List<JSONMember> val) {
        JSONObject obj = new JSONObject();
        for ( JSONMember v : val ) {
            obj.set(v.str.value(), v.value);
        }
        return obj;
    }
    
}
