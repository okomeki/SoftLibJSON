package net.siisise.json.map;

import java.util.UUID;
import net.siisise.omap.MtoConvert;
import net.siisise.omap.OMConvert;

/**
 *
 */
public class JSONUUIDM implements OMConvert {

    @Override
    public Class[] getSrcClasses() {
        return new Class[] { UUID.class };
    }

    @Override
    public <T> T valueOf(Object uuid, MtoConvert<T> outConvert) {
        if (uuid instanceof UUID) {
            return outConvert.stringValue(uuid.toString());
        }
        return (T)this;
    }

}
