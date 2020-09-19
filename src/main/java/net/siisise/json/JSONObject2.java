package net.siisise.json;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonObject;
import javax.json.JsonStructure;
import javax.json.JsonValue.ValueType;
import net.siisise.json.pointer.JSONPointer;

/**
 *
 * @param <K>
 */
public class JSONObject2<K extends String> extends JSONValue<Map<K,JSONValue>> implements JSONCollection<K,Map<K,JSONValue>>,Map<K,JSONValue> {
    
    private List<K> names = new ArrayList();
    
    JSONObject2() {
        value = new HashMap<>();
    }

    @Override
    public Map<String, Object> map() {
        Map<String, Object> ret = new HashMap<>();
        for (String key : value.keySet()) {
            ret.put(key, value.get(key).map());
        }
        return ret;
    }

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
        } else if (cls.isAssignableFrom(JsonObject.class)) {
            return (T) toJson();
        } else if (Map.class.isAssignableFrom(cls)) { // まだ
            Map map = new HashMap();
            for (String key : value.keySet()) {
                if (clss.length == 3) {
                    map.put(key, value.get(key).map(JSONMap.replaces,clss[2]));
                } else {
                    map.put(key, value.get(key).map());
                }
            }
            return (T) map;
        }

        try {
            T obj = cls.getConstructor().newInstance();
            for (String name : names) {
                try {
                    Field f = cls.getField(name);
                    Class<?> typ = f.getType();
                    f.set(obj, value.get(name).map(JSONMap.replaces,typ));
                } catch ( NoSuchFieldException ex ) { // fieldがないときは捨てるかサブクラスを探すか
                    Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
            return obj;
        } catch (NoSuchMethodException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
            throw new java.lang.UnsupportedOperationException(ex);
        } catch (SecurityException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
            throw new java.lang.UnsupportedOperationException(ex);
        } catch (InstantiationException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
            throw new java.lang.UnsupportedOperationException(ex);
        } catch (IllegalAccessException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
            throw new java.lang.UnsupportedOperationException(ex);
        } catch (IllegalArgumentException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
            throw new java.lang.UnsupportedOperationException(ex);
        } catch (InvocationTargetException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
            throw new java.lang.UnsupportedOperationException(ex);
        }
//        throw new java.lang.UnsupportedOperationException("えらー");
    }

    public ValueType getValueType() {
        return ValueType.OBJECT;
    }

    @Override
    public int size() {
        return value.size();
    }

    @Override
    public void clear() {
        value.clear();
    }

    @Override
    public boolean isEmpty() {
        return value.isEmpty();
    }

    @Override
    public JSONValue put(K key, JSONValue value) {
        JSONValue v = this.value.put(key, value);
        if (!names.contains(key)) {
            names.add(key);
        }
        return v;
    }

    @Override
    public boolean containsKey(Object key) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public boolean containsValue(Object value) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public JSONValue get(Object key) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public JSONValue get(String key) {
        return value.get(key);
    }

    @Override
    public JSONValue getJSON(String key) {
        return value.get(key);
    }

    @Override
    public JSONValue getJSON(JSONPointer point) {
        return point.get(this);
    }

    @Override
    public JSONValue remove(Object key) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void putAll(Map<? extends K, ? extends JSONValue> m) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Set<K> keySet() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Collection<JSONValue> values() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Set<Entry<K, JSONValue>> entrySet() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void setJSON(String key, JSONValue obj) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void addJSON(String key, JSONValue obj) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public JSONValue putJSON(String key, JSONValue obj) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public JSONValue removeJSON(String key) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Set keySetJSON() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Iterator<JSONValue> iterator() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void set(String key, Object obj) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void add(String key, Object obj) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Object put(String key, Object obj) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public JSONValue remove(String key) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public JsonStructure toJson() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
}
