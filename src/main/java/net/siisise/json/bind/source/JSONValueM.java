package net.siisise.json.bind.source;

import javax.json.JsonValue;
import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONNULL;
import net.siisise.json.bind.MtoConvert;
import net.siisise.json.bind.OMConvert;

/**
 * Boolean, NULL, JSONValue
 */
public class JSONValueM implements OMConvert {

    @Override
    public Class[] getSrcClasses() {
        return new Class[]{JsonValue.class, JSONNULL.class, JSONBoolean.class, Boolean.class};
    }

    @Override
    public Object valueOf(Object obj, MtoConvert outConvert) {
        if (obj == null || obj == JsonValue.NULL || obj instanceof JSONNULL ) {
            return outConvert.nullValue();
        } else if (outConvert.targetClass() instanceof Class && ((Class) outConvert.targetClass()).isInstance(obj)) { // 仮位置
            return obj;
        } else if (obj == JsonValue.TRUE || obj == JSONBoolean.TRUE) {
            return outConvert.booleanValue(true);
        } else if (obj == JsonValue.FALSE || obj == JSONBoolean.FALSE) {
            return outConvert.booleanValue(false);
        } else if (obj instanceof Boolean) {
            return outConvert.booleanValue((boolean) obj);
        }

        return this;
    }

}
