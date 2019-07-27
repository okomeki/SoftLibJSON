package net.siisise.json.parser;

import java.math.BigInteger;
import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFBaseParser;
import net.siisise.io.Packet;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONValue;

/**
 *
 * @author okome
 */
public class JSONNumberP extends ABNFBaseParser<JSONValue, JSONValue> {

    public JSONNumberP(ABNF def, ABNFReg reg, ABNFReg base) {
        super(def, reg);
    }

    /**
     * まだ評価してない
     *
     * @param pac
     * @return
     */
    @Override
    public JSONNumber parse(Packet pac) {
        ABNF.B<Packet> ret = def.find(pac, "frac", "exp");
        if (ret == null) {
            return null;
        }

        //    Packet i = ret.get("int").get(0);
        //    List<Packet> m = ret.get("minus");
        List<Packet> f = ret.get("frac"); // 小数点
        List<Packet> e = ret.get("exp"); // 浮動小数点
        if (f != null || e != null) {
            return new JSONNumber<>(Double.valueOf(str(ret.ret)));
        } else { // 整数
            return new JSONNumber<>(new BigInteger(str(ret.ret)));
        }

        //return new JSONNumber<>(str(ret.ret));
    }

}
