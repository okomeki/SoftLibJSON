package net.siisise.json2.parser;

import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;
import net.siisise.json2.JSON2Array;

/**
 * JSON2Array の組み立て.
 * abnfでJSON のvalueを抽出して格納するだけ.
 */
public class JSON2ArrayP extends ABNFList<JSON2Array, Object> {
    
    public JSON2ArrayP(ABNF rule, ABNFReg base) {
        super(rule, base, "value");
    }

    @Override
    protected JSON2Array build(List<Object> val) {
        return new JSON2Array(val);
    }
}
