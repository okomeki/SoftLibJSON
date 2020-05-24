package net.siisise.json.jsonp;

import java.util.Collection;
import java.util.Map;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonBuilderFactory;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONPBuilderFactory implements JsonBuilderFactory {

    @Override
    public JsonObjectBuilder createObjectBuilder() {
        return new JSONPObjectBuilder();
    }

    @Override
    public JsonObjectBuilder createObjectBuilder(JsonObject object) {
        JsonObjectBuilder ob = createObjectBuilder();
        for (String name : object.keySet()) {
            ob.add(name, object.get(name));
        }
        return ob;
    }

    @Override
    public JsonObjectBuilder createObjectBuilder(Map<String, Object> object) {
        JsonObjectBuilder ob = createObjectBuilder();
        for (String name : object.keySet()) {
            ob.add(name, JSONValue.valueOf(object.get(name)).toJson());
        }
        return ob;
    }

    @Override
    public JsonArrayBuilder createArrayBuilder() {
        return new JSONPArrayBuilder();
    }

    @Override
    public JsonArrayBuilder createArrayBuilder(JsonArray array) {
        JsonArrayBuilder ab = createArrayBuilder();
        for (JsonValue v : array) {
            ab.add(v);
        }
        return ab;
    }

    @Override
    public JsonArrayBuilder createArrayBuilder(Collection<?> collection) {
        JsonArrayBuilder ab = createArrayBuilder();
        for (Object o : collection) {
            ab.add(JSONValue.valueOf(o).toJson());
        }
        return ab;
    }

    @Override
    public Map<String, ?> getConfigInUse() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

}