package net.siisise.json2;

import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import javax.json.JsonNumber;
import javax.json.JsonValue;
import net.siisise.json.JSONFormat;

/**
 * Number を扱う.
 * データ型は特定のものを想定していないのでいろいろ。
 * これもNumver型になれるので多重ラップしないよう注意。
 */
public class JSON2Number extends Number implements JSON2Value,JsonNumber {
    
    private final Number number;
    
    public JSON2Number(Number num) {
        number = num;
    }

    @Override
    public int intValue() {
        return number.intValue();
    }

    @Override
    public long longValue() {
        return number.longValue();
    }

    @Override
    public float floatValue() {
        return number.floatValue();
    }

    @Override
    public double doubleValue() {
        return number.doubleValue();
    }

    @Override
    public <T> T typeMap(Type type) {
        if ( !(type instanceof Class) ) {
            throw new UnsupportedOperationException("まだ");
        }
        Class<T> cls = (Class)type;
        
        if ( cls.isInstance(number)) {
            return (T)number;
        }

        if (cls == Integer.TYPE || cls == Integer.class) {
            return (T) Integer.valueOf(number.intValue());
        } else if (cls == Long.TYPE || cls == Long.class) {
            return (T) Long.valueOf(number.longValue());
        } else if (cls == Short.TYPE || cls == Short.class) {
            return (T) Short.valueOf(number.shortValue());
        } else if (cls == Character.TYPE || cls == Character.class) {
            return (T) Character.valueOf((char)number.intValue());
        } else if (cls == Byte.TYPE || cls == Byte.class) {
            return (T) Byte.valueOf(number.byteValue());
        } else if (cls == Float.TYPE || cls == Float.class) {
            return (T) Float.valueOf(number.floatValue());
        } else if (cls == Double.TYPE || cls == Double.class) {
            return (T) Double.valueOf(number.doubleValue());
        } else if (cls == BigInteger.class) {
            return (T) new BigInteger(number.toString());
        } else if (cls == BigDecimal.class) {
            return (T) new BigDecimal(number.toString());
        } else if ( cls == String.class || cls == CharSequence.class ) {
            return (T)number.toString();
        }

        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public JsonValue toJson() {
        return this;
    }
    
    @Override
    public String toString() {
        return toString(NOBR);
    }

    @Override
    public String toString(JSONFormat format) {
        return number.toString();
    }

    @Override
    public <T> T map() {
        return (T)number;
    }

    @Override
    public boolean isIntegral() {
        if (number instanceof Integer || number instanceof Long || number instanceof Short || number instanceof Byte || number instanceof BigInteger) {
            return true;
        }
        if (number instanceof Float || number instanceof Double || number instanceof BigDecimal) {
            return false;
        }
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public int intValueExact() {
        return typeMap(Integer.class);
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
    public BigDecimal bigDecimalValue() {
        return typeMap(BigDecimal.class);
    }

    @Override
    public ValueType getValueType() {
        return ValueType.NUMBER;
    }
    
}
