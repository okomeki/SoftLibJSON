package net.siisise.json2.parser;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFBuildParser;
import net.siisise.io.Packet;

/**
 * JSON Number の組み立て
 * 浮動小数点型は IEEE 754
 * Java の Number 型でまとめる
 */
public class JSON2NumberP extends ABNFBuildParser<Number, Packet> {

    public JSON2NumberP(ABNF rule, ABNFReg base) {
        super(rule, null, base, "frac", "exp");
    }

    @Override
    protected Number build(ABNF.C<Packet> ret) {

        List f = ret.get("frac"); // 小数点
        List e = ret.get("exp"); // 浮動小数点
        if (f != null || e != null) {
            return new BigDecimal(str(ret.ret));
        } else { // 整数
            return new BigInteger(str(ret.ret));
        }
    }
    
}
