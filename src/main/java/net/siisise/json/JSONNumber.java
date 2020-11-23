package net.siisise.json;

import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import javax.json.JsonNumber;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.AbstractABNF;
import net.siisise.abnf.parser.ABNFPacketParser;
import net.siisise.io.FrontPacket;
import net.siisise.omap.OMAP;

/**
 * BigDecimal
 *
 * @param <T> Number と String も可能かもしれない(未定)
 */
public class JSONNumber<T> extends JSONValue<T> implements JsonNumber {

    public JSONNumber(T val) {
        value = val;
    }

    @Override
    public Number map() {
        if (value instanceof Number) {
            return (Number) value;
        } else if (value instanceof String) {
            if (((String) value).contains(".")) { // 仮
                return new BigDecimal((String) value);
                //return Double.valueOf((String)value);
            } else {
                BigInteger num = new BigInteger((String) value);
                if (num.compareTo(BigInteger.valueOf(Long.MAX_VALUE)) > 0
                        || num.compareTo(BigInteger.valueOf(Long.MIN_VALUE)) < 0) {
                    return num;
                } else if (num.compareTo(BigInteger.valueOf(Integer.MAX_VALUE)) > 0
                        || num.compareTo(BigInteger.valueOf(Integer.MIN_VALUE)) < 0) {
                    return Long.valueOf((String) value);
                } else {
                    return Integer.valueOf((String) value);
                }
            }
        }
        throw new UnsupportedOperationException();
    }
    
    @Override
    public Number numberValue() {
        return map();
    }

    /**
     * Number系数値に変換
     * 文字の場合はutf-16か
     * 
     * @param <T>
     * @param type
     * @return 
     */
    @Override
    public <T> T typeMap(Type type) {
        Number val;
        if ( value instanceof String ) {
            val = map();
        } else {
            val = (Number)value;
        }
        return (T)OMAP.typeNumber(val, type);
    }

    @Override
    public ValueType getValueType() {
        return ValueType.NUMBER;
    }

    @Override
    public JsonNumber toJson() {
        return this;
    }

    ABNFPacketParser x(ABNF bnf) {
        return new ABNFPacketParser(bnf, JSON8259Reg.REG);
    }

    @Override
    public boolean isIntegral() {
        if (value instanceof Integer || value instanceof Long || value instanceof Short || value instanceof Character || value instanceof Byte || value instanceof BigInteger) {
            return true;
        }
        if (value instanceof Float || value instanceof Double || value instanceof BigDecimal) {
            return false;
        }
        if (value instanceof String) {
            ABNF.C<FrontPacket> r = JSON8259Reg.number.find(AbstractABNF.pac((String) value), x(JSON8259Reg.exp), x(JSON8259Reg.frac));
            if (r != null) {
                List<FrontPacket> exp = r.get(JSON8259Reg.exp);
                List<FrontPacket> frag = r.get(JSON8259Reg.frac);
                return exp == null && frag == null;
            }
        }
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public int intValue() {
        return typeMap(Integer.TYPE);
    }

    @Override
    public int intValueExact() {
        return typeMap(Integer.class);
    }

    /**
     * 精度落ちがあるかもしれない。
     *
     * @return
     */
    @Override
    public long longValue() {
        return typeMap(Long.TYPE);
    }

    @Override
    public long longValueExact() {
        return typeMap(Long.class);
    }

    @Override
    public BigInteger bigIntegerValue() {
        return typeMap(BigInteger.class);
    }

    @Override
    public BigInteger bigIntegerValueExact() {
        return typeMap(BigInteger.class);
    }

    @Override
    public double doubleValue() {
        return typeMap(Double.TYPE);
    }

    @Override
    public BigDecimal bigDecimalValue() {
        return typeMap(BigDecimal.class);
    }
}
