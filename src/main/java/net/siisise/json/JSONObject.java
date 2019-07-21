package net.siisise.json;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author okome
 */
public class JSONObject extends JSONCollection<Map<String, JSONValue>> {

    Map<String, JSONValue> values = new HashMap<>();
    List<String> names = new ArrayList<>();

    @Override
    public Map<String, JSONValue> value() {
        Map<String, JSONValue> ret = new HashMap<>();
        for (String key : values.keySet()) {
            ret.put(key, values.get(key));
        }
        return ret;
    }

    @Override
    public Map<String, Object> map() {
        Map<String, Object> ret = new HashMap<>();
        for (String key : values.keySet()) {
            ret.put(key, values.get(key).map());
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
            for (String key : values.keySet()) {
                if (clss.length == 3) {
                    map.put(key, values.get(key).map(clss[2]));
                } else {
                    map.put(key, values.get(key).map());
                }
            }
            return (T) map;
        }

        try {
            T obj = cls.getConstructor().newInstance();
            for (String name : names) {
                Field f = cls.getField(name);
                Class<?> typ = f.getType();
                f.set(obj, values.get(name).map(typ));
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
        return values.get((String)key);
    }

    @Override
    public void set(String key, Object value) {
        values.put(key, valueOf(value));
        if (!names.contains(key)) {
            names.add(key);
        }
    }

    @Override
    public void add(String key, Object value) {
        set(key, value);
    }

    @Override
    public JSONValue remove(Object key) {
        if (names.contains(key)) {
            names.remove(key);
            return values.remove(key);
        } else {
            throw new java.lang.UnsupportedOperationException();
        }
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("{\r\n");
        for (String name : names) {
            if (sb.length() > 3) {
                sb.append(",\r\n");
            }
            sb.append("  \"");
            sb.append(name);
            sb.append("\":");
            sb.append(tab(values.get(name).toString()));
        }
        sb.append("\r\n}");
        return sb.toString();
    }

    /**
     *
     * @param map
     * @return
     */
    public static JSONObject convMap(Map map) {
        JSONObject jo = new JSONObject();
        for (Object key : map.keySet()) {
            JSONValue vo = valueOf(map.get(key));
            if (key instanceof String) {
                jo.set((String) key, vo);
            } else {
                jo.set(key.toString(), vo);
            }
        }
        return jo;
    }

    public static JSONObject convObject(Object obj) {
        Class<? extends Object> c = obj.getClass();
        Field[] fs = c.getFields();
        JSONObject jo = new JSONObject();
        String name;
        for (Field f : fs) {
            name = f.getName();
            try {
                jo.set(name, valueOf(f.get(obj)));
            } catch (IllegalArgumentException ex) {
                Logger.getLogger(JSON8259Reg.class.getName()).log(Level.SEVERE, null, ex);
            } catch (IllegalAccessException ex) {
                Logger.getLogger(JSON8259Reg.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return jo;
    }

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
     * @return 
     */
    @Override
    public int size() {
        return values.size();
    }
    
    @Override
    public Set<String> keySet() {
        return value.keySet();
    }
    
    @Override
    public boolean isEmpty() {
        return value.isEmpty();
    }

    @Override
    public void clear() {
        value.clear();
    }
}
