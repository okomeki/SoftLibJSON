package net.siisise.json.map;

import java.util.UUID;
import net.siisise.json.JSONReplace;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONUUID implements JSONReplace<UUID> {

    @Override
    public Class<UUID> targetClass() {
        return UUID.class;
    }
    
    public static JSONValue valueOf(Object uuid) {
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
