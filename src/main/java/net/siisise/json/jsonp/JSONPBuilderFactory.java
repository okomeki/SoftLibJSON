package net.siisise.json.jsonp;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonBuilderFactory;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
import net.siisise.bind.Rebind;

/**
 *
 */
public class JSONPBuilderFactory implements JsonBuilderFactory {

    public JSONPBuilderFactory(Map<String, ?> map) {
//        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public JsonObjectBuilder createObjectBuilder() {
        return new JSONPObjectBuilder();
    }

    @Override
    public JsonObjectBuilder createObjectBuilder(JsonObject object) {
        JsonObjectBuilder ob = createObjectBuilder();
        object.forEach((k,v) -> {ob.add(k, (JsonValue)Rebind.valueOf(v, JsonValue.class));});
        return ob;
    }

    @Override
    public JsonObjectBuilder createObjectBuilder(Map<String, Object> object) {
        JsonObjectBuilder ob = createObjectBuilder();
        object.forEach((k,v) -> {ob.add(k, (JsonValue)Rebind.valueOf(v, JsonValue.class));});
        return ob;
    }

    @Override
    public JsonArrayBuilder createArrayBuilder() {
        return new JSONPArrayBuilder();
    }

    @Override
    public JsonArrayBuilder createArrayBuilder(JsonArray array) {
        JsonArrayBuilder ab = createArrayBuilder();
        array.forEach(v -> ab.add((JsonValue)Rebind.valueOf(v, JsonValue.class)));
        return ab;
    }

    @Override
    public JsonArrayBuilder createArrayBuilder(Collection<?> collection) {
        JsonArrayBuilder ab = createArrayBuilder();
        collection.forEach(v -> ab.add((JsonValue)Rebind.valueOf(v, JsonValue.class)));
        return ab;
    }

    @Override
    public Map<String, ?> getConfigInUse() {
        return new HashMap<>();
    }

}
