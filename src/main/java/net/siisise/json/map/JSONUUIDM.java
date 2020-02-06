package net.siisise.json.map;

import java.util.UUID;
import net.siisise.json.JSONReplaceMO;
import net.siisise.json.JSONReplaceOM;
import net.siisise.json.JSONReplacer;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONUUIDM implements JSONReplaceMO<UUID>,JSONReplaceOM {

    @Override
    public Class<UUID> targetClass() {
        return UUID.class;
    }

    @Override
    public JSONValue valueOf(Object uuid, JSONReplacer r) {
        if (uuid instanceof UUID) {
            return JSONValue.valueOf(uuid.toString());
        }
        return null;
    }

    @Override
    public UUID replace(JSONValue json, Class<UUID> target) {
        return UUID.fromString((String) json.value());
    }
    
}
