package net.siisise.json.parser;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import net.siisise.bnf.BNF;
import net.siisise.bnf.BNFReg;
import net.siisise.bnf.parser.BNFBuildParser;
import net.siisise.io.Packet;

/**
 * JSON Number の組み立て
 * 浮動小数点型は IEEE 754
 * Java の Number 型でまとめる
 */
public class JSONNumberP extends BNFBuildParser<Number, Packet> {

    public JSONNumberP(BNF rule, BNFReg base) {
        super(rule, base, "frac", "exp");
    }

    @Override
    protected Number build(BNF.C<Packet> ret) {

        List f = ret.get("frac"); // 小数点
        List e = ret.get("exp"); // 浮動小数点
        if (f != null || e != null) {
            return new BigDecimal(str(ret.ret));
        } else { // 整数
            return new BigInteger(str(ret.ret));
        }
    }
    
}
