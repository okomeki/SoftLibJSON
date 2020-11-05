package net.siisise.json2;

import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import javax.json.JsonValue;
import net.siisise.json.JSONFormat;
import net.siisise.json.JSONNumber;

/**
 * Number を扱う.
 * データ型は特定のものを想定していないのでいろいろ。
 * これもNumver型になれるので多重ラップしないよう注意。
 */
public class JSON2Number extends Number implements JSON2Value {
    
    Number number;
    
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
    
        Number val;
        val = (Number)number;

        if (cls == Integer.TYPE || cls == Integer.class) {
            return (T) Integer.valueOf(val.intValue());
        } else if (cls == Long.TYPE || cls == Long.class) {
            return (T) Long.valueOf(val.longValue());
        } else if (cls == Short.TYPE || cls == Short.class) {
            return (T) Short.valueOf(val.shortValue());
        } else if (cls == Character.TYPE || cls == Character.class) {
            return (T) Character.valueOf((char)val.intValue());
        } else if (cls == Byte.TYPE || cls == Byte.class) {
            return (T) Byte.valueOf(val.byteValue());
        } else if (cls == Float.TYPE || cls == Float.class) {
            return (T) Float.valueOf(val.floatValue());
        } else if (cls == Double.TYPE || cls == Double.class) {
            return (T) Double.valueOf(val.doubleValue());
        } else if (cls == BigInteger.class) {
            return (T) new BigInteger(val.toString());
        } else if (cls == BigDecimal.class) {
            return (T) new BigDecimal(val.toString());
        } else if ( cls == String.class ) {
            return (T)number.toString();
        }

        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public JsonValue toJson() {
        return new JSONNumber(number);
    }

    @Override
    public String toString(JSONFormat format) {
        return number.toString();
    }

    @Override
    public <T> T map() {
        return (T)number;
    }
    
}
