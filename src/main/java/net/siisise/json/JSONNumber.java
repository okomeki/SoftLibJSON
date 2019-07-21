package net.siisise.json;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * BigDecimal
 *
 * @author okome
 * @param <T>
 */
public class JSONNumber<T> extends JSONValue<T> {

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
        throw new java.lang.UnsupportedOperationException();
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
//        } else if ( cls == String.class ) {
//            return (T)map().toString();
        }

        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
