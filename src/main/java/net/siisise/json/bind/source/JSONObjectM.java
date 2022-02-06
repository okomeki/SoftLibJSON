package net.siisise.json.bind.source;

import java.util.HashMap;
import java.util.Map;
import net.siisise.json.JSONObject;
import net.siisise.json.bind.MtoConvert;
import net.siisise.json.bind.OMConvert;

/**
 *
 */
public class JSONObjectM implements OMConvert {

    @Override
    public Class[] getSrcClasses() {
        return new Class[]{JSONObject.class, HashMap.class, Object.class};
    }

    /**
     *
     * @param src
     * @return JSON2Object または primitive型 ?
     */
    @Override
    public Object valueOf(Object src, MtoConvert outConvert) {
        if (src instanceof Map) { // JsonObject, JSON2Object はMap
            return outConvert.mapValue((Map) src);
        }
        return outConvert.objectValue(src);
    }

}
