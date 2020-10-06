package net.siisise.json;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonArray;
import javax.json.JsonValue;
import static net.siisise.json.JSONArray.COLL;
import net.siisise.json.jsonp.JSONPArray;
import net.siisise.json.pointer.JSONPointer;

/**
 *
 */
public class JSONArray2 extends JSONValue<List<JSONValue>> implements JSONCollection<String,List<JSONValue>> {

    public JSONArray2() {
        value = new ArrayList();
    }

    public JSONArray2(Collection list) {
        value = new ArrayList<>();
        list.forEach(val -> {
            value.add(JSON.valueOf(val));
        });
    }

    @Override
    public List<JSONValue> value() {
        return new ArrayList(value);
    }
    
    /**
     * JSON
     * @return 
     */
    @Override
    public List map() {
        List list = new ArrayList();
        value.forEach(json -> {
            list.add(json.map());
        });
        return list;
    }

    /**
     * List または配列にマッピングする.
     * JSON
     *
     * @param <T> 要素型
     * @param cls 変換対象型
     * @return Listまたは配列
     */
    @Override
    public <T> T map(Class<T> cls) {
        return map(new Class[]{cls});
    }

    /**
     * List または　配列
     *
     * @param <T>
     * @param clss Collection または配列、JSONArray、それ以外は要素の型として扱う
     * @return 配列をObject に入れる罠
     */
    @Override
    public <T> T map(Class... clss) {
        Class<T> cls = clss[0];
        if (cls == String.class) {
            return (T) toString();
        } else if (cls.isAssignableFrom(this.getClass())) {
            return (T) this;
        } else if (cls.isAssignableFrom(JsonArray.class)) {
            return (T) toJson();
        }

        if (cls.isArray()) { // 配列 要素の型も指定可能
            Class componentType = cls.getComponentType();
            Object array = (Object) Array.newInstance(componentType, value.size());

            int i = 0;
            for (JSONValue val : value) {
                Array.set(array, i++, val.map(componentType));
            }
            return (T) array;
        }

        // Collection 要素の型は?
        for (Class<? extends Collection> colCls : COLL) {
            if (cls.isAssignableFrom(colCls)) {
                return collectionMap(colCls, clss);
            }
        }
        // ToDo: コンストラクタに突っ込む.
        int len = value.size();
        Constructor[] cnss = cls.getConstructors();
//        List<Constructor> cons = new ArrayList<>();
        
        for ( Constructor c : cnss ) {
            if ( c.getParameterCount() != len ) continue;
            
//            Class[] pt = c.getParameterTypes();
//            Object[] params =  new Object[pt.length];
//            for ( int i = 0; i < pt.length; i++ ) {
//                params[i] = get(i).map(pt[i]);
//            }
            
//            cons.add(c);
//        }
//        if ( cons.size() == 1 ) { // 対象っぽいのがあれば
//            Constructor c = cons.get(0);
            Class[] pt = c.getParameterTypes();
            Object[] params = new Object[pt.length];
            try {
                for ( int i = 0; i < pt.length; i++ ) {
                    params[i] = get(i).map(pt[i]);
                }
                return (T)c.newInstance(params);
            } catch (UnsupportedOperationException ex) {
                Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
            } catch (InstantiationException ex) {
                Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
            } catch (IllegalAccessException ex) {
                Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
            } catch (IllegalArgumentException ex) {
                Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
            } catch (InvocationTargetException ex) {
                Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        
        throw new UnsupportedOperationException();
    }

    private <T> T collectionMap(Class<? extends Collection> colCls, Class... clss) {
        Collection col;

        try {
            col = colCls.getConstructor().newInstance();

            if (clss.length > 1) {
                Class[] clb = new Class[clss.length - 1];
                System.arraycopy(clss, 1, clb, 0, clb.length);

                value.forEach(o -> {
                    if (o instanceof JSONCollection) {
                        col.add(((JSONCollection) o).map(clb));
                    } else {
                        col.add(o.map(clss[1]));
                    }
                });
            } else {
                for (JSONValue o : value) {
                    col.add(o);
                }
            }
            return (T) col;
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
                | InvocationTargetException | NoSuchMethodException | SecurityException ex) {
            Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
        }
        throw new UnsupportedOperationException();
    }


    /**
     * JSON to Json
     * @return 
     */
    @Override
    public JsonArray toJson() {
        JSONPArray ar = new JSONPArray();
        if (value.isEmpty()) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }
        for (JSONValue val : value) {
            ar.add(val.toJson());
        }
        return ar;
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

//    @Override
    public Object[] toArray() {
        return value.toArray();
    }

//    @Override
    public <T> T[] toArray(T[] a) {
        return value.toArray(a);
    }

//    @Override
    public boolean contains(Object o) {
        return value.contains(JSON.valueOf(o));
    }

    @Override
    public Iterator<JSONValue> iterator() {
        return value.iterator();
    }

//    @Override
    public boolean add(JSONValue e) {
        return value.add(JSON.valueOf(e));
    }

    /**
     * @param key index または Pointer
     * @return 
     */
    @Override
    public JSONValue remove(Object key) {
        if (key instanceof Number) {
            key = key.toString();
        } else if (key instanceof JSONPointer) {
            return ((JSONPointer) key).remove(this);
        }
        return value.remove(Integer.parseInt((String) key));
    }

    /** 万能型 */
    @Override
    public JSONValue get(Object key) {
        if (key instanceof Integer) {
            return value.get(((Integer) key));
        } else if (key instanceof Number) {
            return value.get((Integer) ((Number) key).intValue());
        } else if (key instanceof String) {
            return value.get(Integer.parseInt((String) key));
        } else if (key instanceof JSONPointer) {
            return ((JSONPointer) key).get(this);
        }
        return null;
    }

    @Override
    public JSONValue get(String key) {
        return value.get(Integer.parseInt(key));
    }

    // Collection の変換読み
    @Override
    public JSONValue getJSON(String key) {
        return value.get(Integer.parseInt((String) key));
    }
    
    @Override
    public JSONValue getJSON(JSONPointer point) {
        return point.get(this);
    }

    @Override
    public void set(String key, Object obj) {
        setJSON(key, JSON.valueOf(obj));
    }

    @Override
    public void setJSON(String key, JSONValue obj) {
        if (key.equals("-")) {
            value.add(obj);
        } else {
            value.set(Integer.parseInt(key), obj);
        }
    }

    @Override
    public void add(String key, Object obj) {
        JSONValue v = JSON.valueOf(obj);
        addJSON(key, v);
    }

    @Override
    public void addJSON(String key, JSONValue obj) {
        if (key.equals("-")) {
            value.add(obj);
        } else {
            value.add(Integer.parseInt(key), obj);
        }
    }

    @Override
    public Object put(String key, Object obj) {
        return putJSON(key, JSON.valueOf(obj));
    }

    /**
     * Map系
     * @param key
     * @param o
     * @return 
     */
    @Override
    public JSONValue putJSON(String key, JSONValue o) {
        JSONValue v = getJSON(key);
        setJSON(key, o);
        return v;
    }

    @Override
    public JSONValue remove(String key) {
        return removeJSON(key);
    }

    @Override
    public JSONValue removeJSON(String key) {
        return value.remove(Integer.parseInt(key));
    }

    @Override
    public Set<String> keySet() {
        return keySetJSON();
    }

    @Override
    public Set<String> keySetJSON() {
        Set<String> set = new HashSet();
        int size = value.size();
        for (int i = 0; i < size; i++) {
            set.add(Integer.toString(i));
        }
        return set;
    }
}
