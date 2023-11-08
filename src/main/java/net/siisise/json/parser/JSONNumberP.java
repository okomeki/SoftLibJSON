package net.siisise.json.parser;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import net.siisise.bind.format.TypeFormat;
import net.siisise.bnf.BNF;
import net.siisise.bnf.BNFReg;
import net.siisise.bnf.parser.BNFBuildParser;
import net.siisise.io.Packet;

/**
 * JSON Number の組み立て
 * 浮動小数点型は IEEE 754
 * Java の Number 型でまとめる
 */
public class JSONNumberP extends BNFBuildParser<Object, Packet> {

    static final BigInteger MAX_SHORT = BigInteger.valueOf(Short.MAX_VALUE);
    static final BigInteger MIN_SHORT = BigInteger.valueOf(Short.MIN_VALUE);
    static final BigInteger MAX_INTEGER = BigInteger.valueOf(Integer.MAX_VALUE);
    static final BigInteger MIN_INTEGER = BigInteger.valueOf(Integer.MIN_VALUE);
    static final BigInteger MAX_LONG = BigInteger.valueOf(Long.MAX_VALUE);
    static final BigInteger MIN_LONG = BigInteger.valueOf(Long.MIN_VALUE);

    public JSONNumberP(BNF rule, BNFReg base) {
        super(rule, base, "frac", "exp");
    }

    @Override
    protected Object build(BNF.Match<Packet> ret) {
        TypeFormat format = ((UnbindABNFReg)base).getFormat();

        List f = ret.get("frac"); // 小数点
        List e = ret.get("exp"); // 浮動小数点
        if (f != null || e != null) {
            return format.numberFormat(new BigDecimal(str(ret.sub)));
        } else { // 整数
            BigInteger bi = new BigInteger(str(ret.sub));

            if ( bi.compareTo(MAX_SHORT) <= 0 && bi.compareTo(MIN_SHORT) >= 0) {
                return format.numberFormat(bi.shortValue());
            } else if ( bi.compareTo(MAX_INTEGER) <= 0 && bi.compareTo(MIN_INTEGER) >= 0) {
                return format.numberFormat(bi.intValue());
            } else if ( bi.compareTo(MAX_LONG) <= 0 && bi.compareTo(MIN_LONG) >= 0) {
                return format.numberFormat(bi.longValue());
            }
            return format.numberFormat(bi);
        }
    }
    
}
