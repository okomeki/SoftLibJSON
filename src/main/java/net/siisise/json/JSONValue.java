package net.siisise.json;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Map;

/**
 *
 * @author okome
 * @param <T>
 */
public abstract class JSONValue<T> implements JSON<T> {

    T value;

    @Override
    public T value() {
        return value;
    }

    @Override
    public String toString() {
        return value.toString();
    }

    String tab(String val) {
        return val.replace("\r\n", "\r\n  ");
    }

    /**
     *
     * プリミティブ型、配列、Collection、Object boolean byte short char int long float
     * double List Map Number null String
     *
     * @param src データ型なんでも
     * @return
     */
    public static JSONValue valueOf(Object src) {
        if (src == null) {
            return new JSONNULL();
        } else if (src instanceof JSONValue) {
            return (JSONValue) src;
        } else if (src instanceof Number) { // intも
            return new JSONNumber((Number) src);
        } else if (src instanceof Boolean) {
            return new JSONBoolean((Boolean) src);
        } else if (src instanceof String) {
            return new JSONString((String) src);
        } else if (src instanceof Collection) {
            return JSONCollection.convList((Collection) src);
        } else if (src.getClass().isArray()) {
            Class ar = src.getClass().getComponentType();
            List cnv;
            if (ar.isPrimitive()) {
                int len = Array.getLength(src);
                cnv = new ArrayList();
                for (int i = 0; i < len; i++) {
                    cnv.add(Array.get(src, i));
                }
            } else {
                cnv = Arrays.asList((Object[]) src);
            }
            return JSONArray.convList(cnv);
        } else if (src instanceof Map) {
            return JSONObject.convMap((Map) src);
        } else {
            return JSONObject.convObject(src);
        }
    }

    @Override
    public boolean equals(Object o) {
        return o != null && getClass() == o.getClass() && value.equals(((JSONValue) o).value);
    }
}
