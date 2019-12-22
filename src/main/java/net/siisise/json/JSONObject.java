package net.siisise.json;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * public field のみ対応でいい?
 *
 * @see javax.json.JsonObject
 */
public class JSONObject extends JSONCollection<Map<String, JSONValue>> {

    List<String> names = new ArrayList<>();

    public JSONObject() {
        value = new HashMap<>();
    }

    /**
     * 何も考えなくていい変換。
     * 要素の複製はしないことがあるかもしれない
     * @param map
     */
    public JSONObject(Map map) {
        this();
        for (Object key : map.keySet()) {
            Object o = map.get(key);
            if (key instanceof String) {
                put((String) key, o);
            } else {
                put(key.toString(), o);
            }
        }
    }

    @Override
    public Map<String, JSONValue> value() {
        Map<String, JSONValue> ret = new HashMap<>();
        for (String key : value.keySet()) {
            ret.put(key, value.get(key));
        }
        return ret;
    }

    @Override
    public Map<String, Object> map() {
        Map<String, Object> ret = new HashMap<>();
        for (String key : value.keySet()) {
            ret.put(key, value.get(key).map());
        }
        return ret;
    }

    /**
     *
     * @param <T>
     * @param cls
     * @return
     */
    @Override
    public <T> T map(Class<T> cls) {
        return map(new Class[]{cls});
    }

    /**
     * Collection
     *
     * @param <T>
     * @param clss
     * @return
     */
    @Override
    public <T> T map(Class... clss) {
        Class<T> cls = clss[0];
        if (cls == String.class) {
            return (T) toString();
        } else if (cls.isAssignableFrom(this.getClass())) {
            return (T) this;
        } else if (Map.class.isAssignableFrom(cls)) { // まだ
            Map map = new HashMap();
            for (String key : value.keySet()) {
                if (clss.length == 3) {
                    map.put(key, value.get(key).map(clss[2]));
                } else {
                    map.put(key, value.get(key).map());
                }
            }
            return (T) map;
        }

        try {
            T obj = cls.getConstructor().newInstance();
            for (String name : names) {
                Field f = cls.getField(name);
                Class<?> typ = f.getType();
                f.set(obj, value.get(name).map(typ));
            }
            return obj;
        } catch (NoSuchMethodException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
        } catch (SecurityException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InstantiationException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalArgumentException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InvocationTargetException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
        } catch (NoSuchFieldException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
        }
        throw new java.lang.UnsupportedOperationException("えらー");
    }

    @Override
    public JSONValue get(Object key) {
        return value.get((String) key);
    }

    @Override
    public void put(String key, Object value) {
        this.value.put(key, valueOf(value));
        if (!names.contains(key)) {
            names.add(key);
        }
    }

    @Override
    public void set(String key, Object value) {
        put(key, value);
    }

    @Override
    public void add(String key, Object value) {
        put(key, value);
    }

    @Override
    public JSONValue remove(Object key) {
        if (names.contains(key)) {
            names.remove(key);
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
        for (String name : names) {
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
     * toJSON() に対応するかもしれない
     *
     * @param obj
     * @return
     */
    public static JSONValue convObject(Object obj) {
        Class<? extends Object> c = obj.getClass();
        try {
            Method toj = c.getMethod("toJSON");
            String json = (String) toj.invoke(obj);
            return JSON.parse(json);
        } catch (NoSuchMethodException ex) {
            // 特にないので標準の変換へ
        } catch (SecurityException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalAccessException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
        } catch (IllegalArgumentException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
        } catch (InvocationTargetException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
        }
        Field[] fs = c.getFields();
        JSONObject jo = new JSONObject();
        String name;
        for (Field f : fs) {
            name = f.getName();
            try {
                jo.set(name, valueOf(f.get(obj)));
            } catch (IllegalArgumentException ex) {
                Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
            } catch (IllegalAccessException ex) {
                Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return jo;
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
            if (names.size() == obj.names.size() && names.containsAll(obj.names)) {
                for (String name : names) {
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
