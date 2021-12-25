package net.siisise.omap.source;

import java.util.HashMap;
import java.util.Map;
import net.siisise.json2.JSON2Object;
import net.siisise.omap.MtoConvert;
import net.siisise.omap.OMConvert;

/**
 *
 */
public class JSON2ObjectM implements OMConvert {

    @Override
    public Class[] getSrcClasses() {
        return new Class[]{JSON2Object.class, HashMap.class, Object.class};
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
