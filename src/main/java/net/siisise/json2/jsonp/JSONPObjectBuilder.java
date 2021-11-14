package net.siisise.json2.jsonp;

import java.math.BigDecimal;
import java.math.BigInteger;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
import net.siisise.json2.JSON2Number;
import net.siisise.json2.JSON2String;

/**
 * びるだー
 */
public class JSONPObjectBuilder implements JsonObjectBuilder {

    JSONPObject obj = new JSONPObject();

    JSONPObjectBuilder() {
        obj = new JSONPObject();
    }

    JSONPObjectBuilder(JsonObject src) {
        obj = new JSONPObject();
        src.keySet().forEach(name -> obj.put(name, src.get(name)));
    }

    @Override
    public JsonObjectBuilder add(String name, JsonValue value) {
        obj.put(name, value);
        return this;
    }

    @Override
    public JsonObjectBuilder add(String name, String value) {
        obj.put(name, new JSON2String(value));
        return this;
    }

    @Override
    public JsonObjectBuilder add(String name, BigInteger value) {
        obj.put(name, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonObjectBuilder add(String name, BigDecimal value) {
        obj.put(name, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonObjectBuilder add(String name, int value) {
        obj.put(name, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonObjectBuilder add(String name, long value) {
        obj.put(name, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonObjectBuilder add(String name, double value) {
        obj.put(name, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonObjectBuilder add(String name, boolean value) {
        obj.put(name, value ? JsonValue.TRUE : JsonValue.FALSE);
        return this;
    }

    @Override
    public JsonObjectBuilder addNull(String name) {
        obj.put(name, JsonValue.NULL);
        return this;
    }

    @Override
    public JsonObjectBuilder add(String name, JsonObjectBuilder builder) {
        obj.put(name, builder.build());
        return this;
    }

    @Override
    public JsonObjectBuilder add(String name, JsonArrayBuilder builder) {
        obj.put(name, builder.build());
        return this;
    }

    @Override
    public JsonObjectBuilder addAll(JsonObjectBuilder builder) {
        JsonObject src = builder.build();
        src.keySet().forEach(key -> {
            obj.put(key, src.get(key));
        });
        return this;
    }

    @Override
    public JsonObjectBuilder remove(String name) {
        obj.remove(name);
        return this;
    }

    @Override
    public JsonObject build() {
        return obj;
    }

}
