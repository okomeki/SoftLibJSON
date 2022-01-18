package net.siisise.json.map;

import java.util.UUID;
import net.siisise.json.bind.MtoConvert;
import net.siisise.json.bind.OMConvert;

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
