package net.siisise.json2.parser;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFBaseParser;
import net.siisise.io.FrontPacket;
import net.siisise.json2.JSON28259Reg;
import net.siisise.json2.JSON2Value;

/**
 * JSON Number の組み立て
 * 浮動小数点型は IEEE 754
 * Java の Number 型でまとめる
 */
public class JSON2NumberP extends ABNFBaseParser<Number, JSON2Value> {

    public JSON2NumberP(ABNF rule, ABNFReg reg, ABNFReg base) {
        super(rule, reg, base);
    }

    /**
     *
     * @param pac 
     * @return
     */
    @Override
    public Number parse(FrontPacket pac) {
        ABNF.C<FrontPacket> ret = rule.findPacket(pac, JSON28259Reg.frac, JSON28259Reg.exp);
        if (ret == null) {
            return null;
        }

        List<FrontPacket> f = ret.get(JSON28259Reg.frac); // 小数点
        List<FrontPacket> e = ret.get(JSON28259Reg.exp); // 浮動小数点
        if (f != null || e != null) {
            return new BigDecimal(str(ret.ret));
        } else { // 整数
            return new BigInteger(str(ret.ret));
        }
    }
    
}
