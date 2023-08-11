package net.siisise.json.parser;

import java.util.List;
import net.siisise.bnf.BNF;
import net.siisise.bnf.BNFReg;
import net.siisise.bnf.parser.BNFList;
import net.siisise.json.JSONArray;

/**
 * JSON2Array の組み立て.
 * abnfでJSON のvalueを抽出して格納するだけ.
 */
public class JSONArrayP extends BNFList<JSONArray, Object> {
    
    public JSONArrayP(BNF rule, BNFReg base) {
        super(rule, base, "value");
    }

    @Override
    protected JSONArray build(List<Object> val) {
//        return (JSONArray)((UnbindABNFReg)base).getFormat().collectionFormat(val);
        return new JSONArray(val);
    }
}
