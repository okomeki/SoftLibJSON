package net.siisise.json;

import java.lang.reflect.Type;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import javax.json.JsonObject;
import javax.json.JsonValue;
import net.siisise.json2.jsonp.JSONPObject;
import net.siisise.json.pointer.JSONPointer;
import net.siisise.json2.JSON2Object;
import net.siisise.omap.OMAP;

/**
 * JSONのObject
 * JavaのMapに名前順ソートを足したもの。
 * public field のみ対応でいい?
 *
 * @see javax.json.JsonObject
 */
public class JSONObject extends JSONValue<Map<String, JSONValue>> implements JSONCollection<String, Map<String, JSONValue>> {

    public JSONObject() {
        value = new LinkedHashMap<>();
    }

    /**
     * 何も考えなくていい変換。
     * 要素の複製はしないことがあるかもしれない
     *
     * @param map
     */
    public JSONObject(Map map) {
        this();
        map.keySet().forEach(key -> {
            Object o = map.get(key);
            if (key instanceof String) {
                put((String) key, o);
            } else {
                put(key.toString(), o);
            }
        });
    }

    @Override
    public Map<String, JSONValue> value() {
        Map<String, JSONValue> ret = new LinkedHashMap<>();
        value.entrySet().forEach(e -> {
            ret.put(e.getKey(), e.getValue());
        });
        return ret;
    }

    @Override
    public Map<String, Object> map() {
        Map<String, Object> ret = new JSON2Object<>();
        value.entrySet().forEach(e -> {
            ret.put(e.getKey(), e.getValue().map());
        });
        return ret;
    }

    /**
     * Map または Java Objectに変換する。
     * MapのkeyはString またはStringからマッピング可能な何かの予定.
     *
     * @param <T>
     * @param type
     * @return
     */
    @Override
    public <T> T typeMap(Type type) {
        return OMAP.typeMap(this.map(), type);
    }
    
    @Override
    public JsonObject toJson() {
        if (value.isEmpty()) {
            return JsonValue.EMPTY_JSON_OBJECT;
        } else {
            JSONPObject obj = new JSONPObject();
            value.entrySet().forEach(e -> {
                obj.put(e.getKey(), e.getValue().toJson());
            });
            return obj;
        }
    }

    @Override
    public JSONValue get(Object key) {
        if (key instanceof JSONPointer) {
            return ((JSONPointer) key).get(this);
        }
        return value.get((String) key);
    }

    @Override
    public JSONValue get(String key) {
        return value.get(key);
    }

    @Override
    public JSONValue getJSON(JSONPointer point) {
        return point.get(this);
    }

    @Override
    public JSONValue getJSON(String key) {
        return value.get(key);
    }

    @Override
    public JSONValue put(String key, Object obj) {
        return value.put(key, JSON.valueOf(obj));
    }

    /**
     * Map系
     *
     * @param key
     * @param obj
     * @return
     */
    @Override
    public JSONValue putJSON(String key, JSONValue obj) {
        return value.put(key, obj);
    }

    @Override
    public void set(String key, Object value) {
        put(key, value);
    }

    @Override
    public void setJSON(String key, JSONValue obj) {
        putJSON(key, obj);
    }

    @Override
    public void add(String key, Object value) {
        put(key, value);
    }

    @Override
    public void addJSON(String key, JSONValue obj) {
        putJSON(key, obj);
    }

    @Override
    public JSONValue remove(Object key) {
        if (value.containsKey(key)) {
            return value.remove(key);
        } else {
            throw new java.lang.UnsupportedOperationException();
        }
    }

    @Override
    public JSONValue remove(String key) {
        if (value.containsKey(key)) {
            return value.remove(key);
        } else {
            throw new java.lang.UnsupportedOperationException();
        }
    }

    /**
     *
     * @param key
     * @return
     */
    @Override
    public JSONValue removeJSON(String key) {
        if (value.containsKey(key)) {
            return value.remove(key);
        } else {
            throw new java.lang.UnsupportedOperationException();
        }
    }

    @Override
    public String toString(JSONFormat format) {
        StringBuilder sb = new StringBuilder();
        sb.append("{");
        sb.append(format.crlf);
        for (String name : keySet()) {
            if (sb.length() > 3) {
                sb.append(",");
                sb.append(format.crlf);
            }
            sb.append(format.tab);
            sb.append("\"");
            sb.append(name);
            sb.append("\":");
            sb.append(tab(value.get(name).toString()));
        }
        sb.append(format.crlf);
        sb.append("}");
        return sb.toString();
    }

    /**
     *
     * @param o
     * @return
     */
    @Override
    public boolean equals(Object o) {
        if (o instanceof JSONObject) {
            JSONObject obj = (JSONObject) o;
            if (value.size() == obj.value.size() && keySet().containsAll(obj.value.keySet())) {
                for (String name : keySet()) {
                    if (!get(name).equals(obj.get(name))) {
                        return false;
                    }
                }
                return true;
            }
        }
        return false;
    }

    /**
     * Collection 機能の実装
     *
     * @return 項目数
     */
    @Override
    public int size() {
        return value.size();
    }

    /**
     * Mapとおなじ
     *
     * @return 名前のセット
     */
    @Override
    public Set<String> keySet() {
        return value.keySet();
    }

    @Override
    public Set<String> keySetJSON() {
        return value.keySet();
    }

    /**
     * ?
     *
     * @return
     */
    @Override
    public boolean isEmpty() {
        return value.isEmpty();
    }

    /**
     * ?
     */
    @Override
    public void clear() {
        value.clear();
    }

    @Override
    public Iterator<JSONValue> iterator() {
        return value.values().iterator();
    }

}
