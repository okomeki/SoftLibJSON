package net.siisise.json.parser;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFBuildParser;
import net.siisise.io.FrontPacket;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONNumberP extends ABNFBuildParser<JSONValue, FrontPacket> {

    public JSONNumberP(ABNF rule, ABNFReg base) {
        super(rule, base, "frac", "exp");
    }

    /**
     * まだ評価してない
     *
     * @param ret
     * @return
     */
    @Override
    protected JSONNumber build(ABNF.C<FrontPacket> ret) {
        List<FrontPacket> frac = ret.get("frac"); // 小数点
        List<FrontPacket> exp = ret.get("exp"); // 浮動小数点
        if (frac != null || exp != null) {
            return new JSONNumber<>(new BigDecimal(str(ret.ret)));
//            return new JSONNumber<>(Double.valueOf(str(ret.ret)));
        } else { // 整数
            return new JSONNumber<>(new BigInteger(str(ret.ret)));
        }
    }
}
