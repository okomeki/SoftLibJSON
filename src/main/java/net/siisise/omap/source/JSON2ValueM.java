package net.siisise.omap.source;

import javax.json.JsonValue;
import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONNULL;
import net.siisise.json2.JSON2Boolean;
import net.siisise.json2.JSON2NULL;
import net.siisise.omap.MtoConvert;
import net.siisise.omap.OMConvert;

/**
 * Boolean, NULL, JSONValue
 */
public class JSON2ValueM implements OMConvert {

    @Override
    public Class[] getSrcClasses() {
        return new Class[]{JsonValue.class, JSONNULL.class, JSON2NULL.class, JSONBoolean.class, JSON2Boolean.class, Boolean.class};
    }

    @Override
    public Object valueOf(Object obj, MtoConvert outConvert) {
        if (obj == null || obj == JsonValue.NULL || obj instanceof JSON2NULL || obj instanceof JSONNULL ) {
            return outConvert.nullValue();
        } else if (outConvert.targetClass() instanceof Class && ((Class) outConvert.targetClass()).isInstance(obj)) { // 仮位置
            return obj;
        } else if (obj == JsonValue.TRUE || obj == JSON2Boolean.TRUE || obj == JSONBoolean.TRUE) {
            return outConvert.booleanValue(true);
        } else if (obj == JsonValue.FALSE || obj == JSON2Boolean.FALSE || obj == JSONBoolean.FALSE) {
            return outConvert.booleanValue(false);
        } else if (obj instanceof Boolean) {
            return outConvert.booleanValue((boolean) obj);
        }

        return this;
    }

}
