package net.siisise.json2.parser;

import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;
import net.siisise.json2.JSON2Array;

/**
 *
 */
public class JSON2ArrayP extends ABNFList<JSON2Array, Object> {
    
    public JSON2ArrayP(ABNF rule, ABNFReg reg, ABNFReg base) {
        super(rule, reg, base, "value");
    }

    @Override
    protected JSON2Array parse(List<Object> val) {
        return new JSON2Array(val);
    }
}
