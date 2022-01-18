package net.siisise.json;

import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import javax.json.JsonNumber;
import javax.json.JsonValue;
import net.siisise.json.bind.OMAP;

/**
 * Number を扱う.
 * 値の変更は不可.
 * データ型は特定のものを想定していないのでいろいろ。
 * これもNumver型になれるので多重ラップしないよう注意。
 */
public class JSON2Number extends Number implements JSON2Value,JsonNumber {
    
    private final Number number;
    
    public JSON2Number(Number num) {
        number = num;
    }

    @Override
    public Number numberValue() {
        return number;
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
        return (T) OMAP.typeNumber(number, type);
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
    public String toString(JSON2Format format) {
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
        return ((BigDecimal)typeMap(BigDecimal.class)).intValueExact();
    }

    @Override
    public long longValueExact() {
        return ((BigDecimal)typeMap(BigDecimal.class)).longValueExact();
    }

    @Override
    public BigInteger bigIntegerValue() {
        return typeMap(BigInteger.class);
    }

    @Override
    public BigInteger bigIntegerValueExact() {
        return ((BigDecimal)typeMap(BigDecimal.class)).toBigIntegerExact();
    }

    @Override
    public BigDecimal bigDecimalValue() {
        return typeMap(BigDecimal.class);
    }

    @Override
    public ValueType getValueType() {
        return ValueType.NUMBER;
    }
    
    @Override
    public boolean equals(Object o) {
        if ( o instanceof JSON2Number ) {
            return toString().equals(o.toString());
        } else if ( o instanceof Number ) {
            return number.equals(o);
        }
        return false;
    }
    
}
