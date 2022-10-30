package net.siisise.json.jsonxp;

import net.siisise.json.jsonp.*;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonBuilderFactory;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
import net.siisise.json.bind.OMAP;

/**
 *
 */
public class JSONXBuilderFactory implements JsonBuilderFactory {

    public JSONXBuilderFactory(Map<String, ?> map) {
//        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    @Override
    public JsonObjectBuilder createObjectBuilder() {
        return new JSONXObjectBuilder();
    }

    @Override
    public JsonObjectBuilder createObjectBuilder(JsonObject object) {
        JsonObjectBuilder ob = createObjectBuilder();
        object.forEach((k,v) -> {ob.add(k, (JsonValue)OMAP.valueOf(v, JsonValue.class));});
        return ob;
    }

    @Override
    public JsonObjectBuilder createObjectBuilder(Map<String, Object> object) {
        JsonObjectBuilder ob = createObjectBuilder();
        object.forEach((k,v) -> {ob.add(k, (JsonValue)OMAP.valueOf(v, JsonValue.class));});
        return ob;
    }

    @Override
    public JsonArrayBuilder createArrayBuilder() {
        return new JSONPArrayBuilder();
    }

    @Override
    public JsonArrayBuilder createArrayBuilder(JsonArray array) {
        JsonArrayBuilder ab = createArrayBuilder();
        array.forEach(v -> ab.add((JsonValue)OMAP.valueOf(v, JsonValue.class)));
        return ab;
    }

    @Override
    public JsonArrayBuilder createArrayBuilder(Collection<?> collection) {
        JsonArrayBuilder ab = createArrayBuilder();
        collection.forEach(v -> ab.add((JsonValue)OMAP.valueOf(v, JsonValue.class)));
        return ab;
    }

    @Override
    public Map<String, ?> getConfigInUse() {
        return new HashMap<>();
    }

}
