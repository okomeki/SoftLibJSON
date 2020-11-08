package net.siisise.json.jsonp;

import java.math.BigDecimal;
import java.math.BigInteger;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonObjectBuilder;
import javax.json.JsonValue;
import net.siisise.json2.JSON2Boolean;
import net.siisise.json2.JSON2Number;
import net.siisise.json2.JSON2String;

/**
 *
 */
public class JSONPArrayBuilder implements JsonArrayBuilder {

    private JSONPArray array = new JSONPArray();

    @Override
    public JsonArrayBuilder add(JsonValue val) {
        array.add(val);
        return this;
    }

    @Override
    public JsonArrayBuilder add(String val) {
        array.add(new JSON2String(val));
        return this;
    }

    @Override
    public JsonArrayBuilder add(BigDecimal val) {
        array.add(new JSON2Number(val));
        return this;
    }

    @Override
    public JsonArrayBuilder add(BigInteger val) {
        array.add(new JSON2Number(val));
        return this;
    }

    @Override
    public JsonArrayBuilder add(int val) {
        array.add(new JSON2Number(val));
        return this;
    }

    @Override
    public JsonArrayBuilder add(long val) {
        array.add(new JSON2Number(val));
        return this;
    }

    @Override
    public JsonArrayBuilder add(double val) {
        array.add(new JSON2Number(val));
        return this;
    }

    @Override
    public JsonArrayBuilder add(boolean val) {
        array.add(new JSON2Boolean(val).toJson());
        return this;
    }

    @Override
    public JsonArrayBuilder addNull() {
        array.add(JsonValue.NULL);
        return this;
    }

    @Override
    public JsonArrayBuilder add(JsonObjectBuilder job) {
        array.add(job.build());
        return this;
    }

    @Override
    public JsonArrayBuilder add(JsonArrayBuilder jab) {
        array.add(jab.build());
        return this;
    }

    @Override
    public JsonArray build() {
        return array;
    }

    @Override
    public JsonArrayBuilder addAll(JsonArrayBuilder builder) {
        JsonArray ar = builder.build();
        array.addAll(ar);
        return this;
    }

    @Override
    public JsonArrayBuilder add(int index, JsonValue value) {
        array.add(index,value);
        return this;
    }

    @Override
    public JsonArrayBuilder add(int index, String value) {
        array.add(index, new JSON2String(value));
        return this;
    }

    @Override
    public JsonArrayBuilder add(int index, BigDecimal value) {
        array.add(index, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonArrayBuilder add(int index, BigInteger value) {
        array.add(index, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonArrayBuilder add(int index, int value) {
        array.add(index, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonArrayBuilder add(int index, long value) {
        array.add(index, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonArrayBuilder add(int index, double value) {
        array.add(index, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonArrayBuilder add(int index, boolean value) {
        array.add(index, value ? JsonValue.TRUE : JsonValue.FALSE);
        return this;
    }

    @Override
    public JsonArrayBuilder addNull(int index) {
        array.add(index, JsonValue.NULL);
        return this;
    }

    @Override
    public JsonArrayBuilder add(int index, JsonObjectBuilder builder) {
        array.add(index, builder.build());
        return this;
    }

    @Override
    public JsonArrayBuilder add(int index, JsonArrayBuilder builder) {
        array.add(index, builder.build());
        return this;
    }

    @Override
    public JsonArrayBuilder set(int index, JsonValue value) {
        array.set(index, value);
        return this;
    }

    @Override
    public JsonArrayBuilder set(int index, String value) {
        array.set(index, new JSON2String(value));
        return this;
    }

    @Override
    public JsonArrayBuilder set(int index, BigDecimal value) {
        array.set(index, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonArrayBuilder set(int index, BigInteger value) {
        array.set(index, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonArrayBuilder set(int index, int value) {
        array.set(index, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonArrayBuilder set(int index, long value) {
        array.set(index, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonArrayBuilder set(int index, double value) {
        array.set(index, new JSON2Number(value));
        return this;
    }

    @Override
    public JsonArrayBuilder set(int index, boolean value) {
        array.set(index, value ? JsonValue.TRUE : JsonValue.FALSE);
        return this;
    }

    @Override
    public JsonArrayBuilder setNull(int index) {
        array.set(index, JsonValue.NULL);
        return this;
    }

    @Override
    public JsonArrayBuilder set(int index, JsonObjectBuilder builder) {
        array.set(index,builder.build());
        return this;
    }

    @Override
    public JsonArrayBuilder set(int index, JsonArrayBuilder builder) {
        array.set(index,builder.build());
        return this;
    }

    @Override
    public JsonArrayBuilder remove(int index) {
        array.remove(index);
        return this;
    }
    
}
