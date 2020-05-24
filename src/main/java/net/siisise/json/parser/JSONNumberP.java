package net.siisise.json.parser;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFBaseParser;
import net.siisise.io.FrontPacket;
import net.siisise.io.Packet;
import net.siisise.json.JSON8259Reg;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONNumberP extends ABNFBaseParser<JSONValue, JSONValue> {

    public JSONNumberP(ABNF def, ABNFReg reg, ABNFReg base) {
        super(def, reg, base);
    }

    /**
     * まだ評価してない
     *
     * @param pac
     * @return
     */
    @Override
    public JSONNumber parse(FrontPacket pac) {
        ABNF.C<Packet> ret = find(pac, JSON8259Reg.frac, JSON8259Reg.exp);
        if (ret == null) {
            return null;
        }

        //    Packet i = ret.get("int").get(0);
        //    List<Packet> m = ret.get("minus");
        List<Packet> f = ret.get(JSON8259Reg.frac); // 小数点
        List<Packet> e = ret.get(JSON8259Reg.exp); // 浮動小数点
        if (f != null || e != null) {
            return new JSONNumber<>(new BigDecimal(str(ret.ret)));
//            return new JSONNumber<>(Double.valueOf(str(ret.ret)));
        } else { // 整数
            return new JSONNumber<>(new BigInteger(str(ret.ret)));
        }
    }
}
