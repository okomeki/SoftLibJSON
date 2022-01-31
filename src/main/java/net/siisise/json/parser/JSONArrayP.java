package net.siisise.json.parser;

import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.bnf.parser.BNFList;
import net.siisise.json.JSON2Array;

/**
 * JSON2Array の組み立て.
 * abnfでJSON のvalueを抽出して格納するだけ.
 */
public class JSONArrayP extends BNFList<JSON2Array, Object> {
    
    public JSONArrayP(ABNF rule, ABNFReg base) {
        super(rule, base, "value");
    }

    @Override
    protected JSON2Array build(List<Object> val) {
        return new JSON2Array(val);
    }
}
