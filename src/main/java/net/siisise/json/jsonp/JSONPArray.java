package net.siisise.json.jsonp;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import net.siisise.json.JSON2Array;
import net.siisise.json.JSON2Boolean;
import net.siisise.json.JSON2NULL;

/**
 *
 */
public class JSONPArray extends JSON2Array<JsonValue> implements JsonArray {

    public JSONPArray() {
        super(JsonValue.class);
    }
    
    public JSONPArray(Collection<JsonValue> col) {
        super(col);
    }
    
    @Override
    public ValueType getValueType() {
        return ValueType.ARRAY;
    }

    @Override
    public boolean isNull(int i) {
        return get(i) == JsonValue.NULL || get(i) == JSON2NULL.NULL;
    }

    @Override
    public JsonObject getJsonObject(int i) {
        return (JsonObject) get(i);
    }

    @Override
    public JsonArray getJsonArray(int i) {
        return (JsonArray) get(i);
    }

    @Override
    public JsonNumber getJsonNumber(int i) {
        return (JsonNumber) get(i);
    }

    @Override
    public JsonString getJsonString(int i) {
        return (JsonString) get(i);
    }

    @Override
    public String getString(int i) {
        return getJsonString(i).getString();
    }

    @Override
    public String getString(int i, String string) {
        JsonString v = getJsonString(i);
        return v == null ? string : v.getString();
    }

    @Override
    public int getInt(int i) {
        return getJsonNumber(i).intValue();
    }

    @Override
    public int getInt(int i, int i1) {
        JsonNumber val = getJsonNumber(i);
        return val == null ? i1 : val.intValue();
    }

    @Override
    public boolean getBoolean(int i) {
//        get(i).getValueType()
        return get(i) == JsonValue.TRUE || get(i) == JSON2Boolean.TRUE;
    }

    @Override
    public boolean getBoolean(int i, boolean bln) {
        JsonValue val = get(i);
        return (val == null) ? bln : val == JsonValue.TRUE; 
    }

    /**
     *
     * @param <T>
     * @param type
     * @return
     */
    @Override
    public <T extends JsonValue> List<T> getValuesAs(Class<T> type) {
       // List<T> list = new ArrayList<>();
        return (List<T>)stream().collect(Collectors.toList());
        //this.forEach(val -> {
        //    list.add((T)val);
        //});
        //return list;
    }

    /**
     * JSONPArray互換の形式List&lt;JsonValue&gt;で格納する
     * @param <T> だいたいなんでもいけるかもしれない。
     * @return JSONP JsonArray対応型データ
     */
    public static <T> Collector<T,?,JSONPArray> collector() {
        return Collector.of(
                JSONPArray::new,
                JSONPArray::addValue,
                (ls, vals) -> {
                    vals.forEach(ls::addValue);
                    return ls;
                },
                Collector.Characteristics.IDENTITY_FINISH
        );
    }
}
