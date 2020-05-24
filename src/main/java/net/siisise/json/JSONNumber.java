package net.siisise.json;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.List;
import javax.json.JsonNumber;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.AbstractABNF;
import net.siisise.abnf.parser.ABNFPacketParser;
import net.siisise.io.FrontPacket;
import net.siisise.io.Packet;

/**
 * BigDecimal
 *
 * @param <T>
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
    public <T> T map(Class<T> cls) {
//        if (cls == value.getClass()) {
//            return (T)value;
//        }
        if (cls.isInstance(value)) {
            return (T) value;
        } else if (cls == Integer.TYPE) {
            return (T) Integer.valueOf(map().toString());
        } else if (cls == Long.TYPE) {
            return (T) Long.valueOf(map().toString());
        } else if (cls == Short.TYPE) {
            return (T) Short.valueOf(map().toString());
        } else if (cls == Float.TYPE) {
            return (T) Float.valueOf(map().toString());
        } else if (cls == Double.TYPE) {
            return (T) Double.valueOf(map().toString());
        } else if (cls == BigInteger.class) {
            return (T) new BigInteger(map().toString());
        } else if (cls == BigDecimal.class ) {
            return (T) new BigDecimal(map().toString());
//        } else if ( cls == String.class ) {
//            return (T)map().toString();
        }

        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public ValueType getValueType() {
        return ValueType.NUMBER;
    }
    
    @Override
    public JsonNumber toJson() {
        return this;
//        return new JsonNumber(map());
    }

    ABNFPacketParser x(ABNF bnf) {
        return new ABNFPacketParser(bnf,JSON8259Reg.REG);
    }
    
    @Override
    public boolean isIntegral() {
        if ( value instanceof Integer || value instanceof Long || value instanceof Short || value instanceof BigInteger ) {
            return true;
        }
        if ( value instanceof Float || value instanceof Double || value instanceof BigDecimal ) {
            return false;
        }
        if ( value instanceof String ) {
            ABNF.C<FrontPacket> r = JSON8259Reg.number.find(AbstractABNF.pac((String) value), x(JSON8259Reg.exp), x(JSON8259Reg.frac));
            if ( r != null ) {
                List<FrontPacket> exp = r.get(JSON8259Reg.exp);
                List<FrontPacket> frag = r.get(JSON8259Reg.frac);
                return exp == null && frag == null;
            }
        }
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public int intValue() {
        return map().intValue();
    }

    @Override
    public int intValueExact() {
        return map(Integer.class);
    }

    /**
     * 精度落ちがあるかもしれない。
     * @return 
     */
    @Override
    public long longValue() {
        return map().longValue();
    }

    @Override
    public long longValueExact() {
        return map(Long.class);
    }

    @Override
    public BigInteger bigIntegerValue() {
        return map(BigInteger.class);
    }

    @Override
    public BigInteger bigIntegerValueExact() {
        return map(BigInteger.class);
    }

    @Override
    public double doubleValue() {
        return map().doubleValue();
    }

    @Override
    public BigDecimal bigDecimalValue() {
        return map(BigDecimal.class);
    }
}
