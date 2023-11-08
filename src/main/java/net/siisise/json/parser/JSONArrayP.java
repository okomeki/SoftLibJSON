package net.siisise.json.parser;

import java.util.List;
import net.siisise.bnf.BNF;
import net.siisise.bnf.BNFReg;
import net.siisise.bnf.parser.BNFList;

/**
 * JSONArray の組み立て.
 * abnfでJSON のvalueを抽出して格納するだけ.
 */
public class JSONArrayP extends BNFList<Object, Object> {
    
    public JSONArrayP(BNF rule, BNFReg base) {
        super(rule, base, "value");
    }

    @Override
    protected Object build(List<Object> val) {
        return ((UnbindABNFReg)base).getFormat().listFormat(val);
    }
}
