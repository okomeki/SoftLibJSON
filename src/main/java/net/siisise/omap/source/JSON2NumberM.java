package net.siisise.omap.source;

import java.math.BigDecimal;
import java.math.BigInteger;
import javax.json.JsonNumber;
import net.siisise.json2.JSON2Number;
import net.siisise.omap.MtoConvert;
import net.siisise.omap.OMConvert;

/**
 *
 */
public class JSON2NumberM implements OMConvert {

    @Override
    public Class[] getSrcClasses() {
        return new Class[]{JsonNumber.class, JSON2Number.class, Number.class, Integer.class, Long.class, Float.class, Short.class, BigInteger.class, Double.class, BigDecimal.class};
    }

    @Override
    public Object valueOf(Object obj, MtoConvert target) {
        if (obj instanceof JsonNumber) { // JSONNumber , JSON2Number も該当
            obj = ((JsonNumber) obj).numberValue();
        }
        if (obj instanceof Number) {
            return target.numberValue((Number) obj);
        }
        return this;
    }
}
