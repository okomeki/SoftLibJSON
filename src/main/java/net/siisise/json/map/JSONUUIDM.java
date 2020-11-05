package net.siisise.json.map;

import java.util.UUID;
import net.siisise.json.JSON;
import net.siisise.json.JSONReplaceMO;
import net.siisise.json.JSONReplaceOM;
import net.siisise.json.JSONReplacer;
import net.siisise.json.JSONValue;
import net.siisise.json2.JSON2Value;

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
            return JSON.valueOf(uuid.toString());
        }
        return null;
    }

    @Override
    public UUID replace(JSONValue json, Class<UUID> target) {
        return UUID.fromString((String) json.value());
    }

    @Override
    public UUID replace(JSON2Value json2, Class<UUID> target) {
        return UUID.fromString((String)json2.map());
    }
    
}
