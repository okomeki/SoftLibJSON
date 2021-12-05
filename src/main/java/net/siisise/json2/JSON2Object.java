package net.siisise.json2;

import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.stream.Collectors;
import javax.json.JsonObject;
import javax.json.JsonValue;
import net.siisise.json2.jsonp.JSONPObject;
import net.siisise.omap.OMAP;

/**
 * JavaではMapに相当する中間型。
 * typeMapによりObjectとも対応する。
 * JsonValue以外の型でも持てるのでJSONPに適さない
 * JSONPではBuilder系かもしれない
 *
 * @param <V> JSONで可能な型、または自由?
 */
public class JSON2Object<V> extends LinkedHashMap<String, V> implements JSON2Collection<V> {

    public JSON2Object() {
    }

    public JSON2Object(Map map) {
        map.keySet().forEach(key -> {
//            V o = (V) JSON2.valueMap(map.get(key));
            V o = (V) map.get(key);
            if (key instanceof String) {
                put((String) key, o);
            } else {
                put(key.toString(), o);
            }
        });
    }

    @Override
    public JSON2Value getJSON(String key) {
        return JSON2.valueOf(get(key));
    }

    @Override
    public void setJSON(String key, JSON2Value obj) {
        putJSON(key, obj);
    }

    @Override
    public void addJSON(String key, JSON2Value obj) {
        putJSON(key, obj);
    }

    @Override
    public JSON2Value putJSON(String key, JSON2Value obj) {
        return JSON2.valueOf(put(key, obj.map()));
    }

    @Override
    public JSON2Value removeJSON(String key) {
        if (keySet().contains(key)) {
            return JSON2.valueOf(remove(key));
        }
        return null;
    }
    
    /**
     * 入れ物なので複製しないで自身を返す
     * @return 自身を返す
     */
    @Override
    public HashMap<String, V> map() {
        return this;
    }

    /**
     * 特定の型情報に変換できる場合は変換する。
     * OMAP側に分けてある機能。
     * @param <T> 型
     * @param type 型情報
     * @return 変換されたオブジェクト
     */
    @Override
    public <T> T typeMap(Type type) {
        return OMAP.typeMap(this, type);
    }

    @Override
    public JsonObject toJson() {
        if (isEmpty()) {
            return JsonValue.EMPTY_JSON_OBJECT;
        } else {
            JSONPObject obj = new JSONPObject();
            entrySet().forEach(e -> {
                obj.put(e.getKey(), OMAP.valueOf(e.getValue(), JsonValue.class));
            });
            return obj;
        }
    }

    @Override
    public String toString() {
        return toString(NOBR);
    }

    @Override
    public String toString(JSON2Format format) {
        return keySet().stream().map(key -> {
            return format.crlf + format.tab + new JSON2String(key).toString(format) + ":"
                    + tab(getJSON(key).toString(format));
        }).collect(Collectors.joining(",", "{", format.crlf + "}"));
    }
}
