package net.siisise.omap.source;

import java.util.HashMap;
import java.util.Map;
import net.siisise.json.JSONObject;
import net.siisise.json2.JSON2Object;
import net.siisise.omap.MtoConvert;
import net.siisise.omap.OMConvert;

/**
 *
 */
public class JSON2ObjectM implements OMConvert {

    @Override
    public Class[] getSrcClasses() {
        return new Class[]{JSONObject.class, JSON2Object.class, HashMap.class, Object.class};
    }

    /**
     *
     * @param src
     * @return JSON2Object または primitive型 ?
     */
    @Override
    public Object valueOf(Object src, MtoConvert outConvert) {
        if (src instanceof JSONObject) {
            src = ((JSONObject) src).map();
        }
        if (src instanceof Map) { // JsonObject, JSON2Object はMap
/*
            if ( replacer != null ) {
                Map<String,Object> m = new JSON2Object(); //OMAP.typeToMap(outConvert.targetClass());
                Map<Object,Object> s = (Map)src;
                s.entrySet().stream().forEach(es ->{
                    String key = es.getKey().toString();
                    Object obj = replacer.replacer(key, es.getValue());
                    m.put(key, obj);
                });
                src = m;
            }
             */
            return outConvert.mapValue((Map) src);
        }
        return outConvert.objectValue(src);
    }

}
