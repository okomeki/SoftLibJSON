package net.siisise.json.parser;

import net.siisise.block.ReadableBlock;
import net.siisise.bnf.BNF;
import net.siisise.bnf.BNFReg;
import net.siisise.bnf.parser.BNFSelect;

/**
 * valueに該当する型で返す.
 * true, false, null はJSONValue型に変換する、他は変換済みなのでそのまま通す.
 */
public class JSONValueP extends BNFSelect<Object> {
    
    public JSONValueP(BNF rule, BNFReg base) {
        super(rule, base, "object", "array", "number", "string");
    }
    
    @Override
    protected Object other(ReadableBlock pac) {
        ReadableBlock p = JSON8259Reg.FALSE.is(pac);
        if (p != null) {
            return ((UnbindABNFReg)base).getFormat().booleanFormat(false);
        }
        p = JSON8259Reg.NULL.is(pac);
        if (p != null) {
            return ((UnbindABNFReg)base).getFormat().nullFormat();
        }
        p = JSON8259Reg.TRUE.is(pac);
        if (p != null) {
            return ((UnbindABNFReg)base).getFormat().booleanFormat(true);
        }
        return null;
    }
}
