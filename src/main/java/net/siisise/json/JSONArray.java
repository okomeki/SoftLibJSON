package net.siisise.json;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonString;
import javax.json.JsonValue;
import net.siisise.json.jsonp.JSONPArray;
import net.siisise.json.pointer.JSONPointer;

/**
 * 配列またはList。 数字をKeyとした疑似Mapとして操作できる
 *
 * @see javax.json.JsonArray
 */
public class JSONArray extends JSONCollection<List<JSONValue>> {

    public JSONArray() {
        value = new ArrayList<>();
    }

    public JSONArray(Collection list) {
        value = new ArrayList<>();
        for (Object val : list) {
            value.add(valueOf(val));
        }
    }

    @Override
    public List<JSONValue> value() {
        return new ArrayList(value);
    }

    /**
     * 要素に変換したもの
     *
     * @return
     */
    @Override
    public List map() {
        List list = new ArrayList();
        for (JSONValue val : value) {
            list.add(val.map());
        }
        return list;
    }

    static Class<? extends Collection>[] COLL = new Class[]{
        ArrayList.class, HashSet.class, LinkedList.class};

    /**
     * List または配列にマッピングする.
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

    private <T> T collectionMap(Class<? extends Collection> colCls, Class... clss) {
        Collection col;

        try {
            col = colCls.getConstructor().newInstance();

            if (clss.length > 1) {
                Class[] clb = new Class[clss.length - 1];
                System.arraycopy(clss, 1, clb, 0, clb.length);

                for (JSONValue o : value) {
                    if (o instanceof JSONCollection) {
                        col.add(((JSONCollection) o).map(clb));
                    } else {
                        col.add(o.map(clss[1]));
                    }
                }
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
     * JSONValueの配列。
     *
     * @return
     */
    public Object[] toArray() {
        return value.toArray();
    }

    /**
     * 特定の型に変換された配列
     *
     * @param <T>
     * @param a
     * @return
     */
    public <T> T[] toArray(T[] a) {
        Class<?> contentType = a.getClass().getComponentType();
        T[] array;
        if (a.length != value.size()) {
            array = (T[]) Array.newInstance(contentType, value.size());
        } else {
            array = a;
        }

        if (contentType == JSONValue.class) {
            return value.toArray(array);
        }

        int i = 0;
        for (JSONValue val : value) {
            array[i++] = (T) val.map(contentType);
        }
        return (T[]) array;
    }

    public void add(Object o) {
        value.add(valueOf(o));
    }

    public JSONValue get(int index) {
        return value.get(index);
    }

    @Override
    public JSONValue get(Object key) {
        if (key instanceof Integer) {
            return value.get(((Integer) key));
        } else if (key instanceof Number) {
            return value.get((Integer) ((Number) key).intValue());
        }
        if (key instanceof String) {
            return value.get(Integer.parseInt((String) key));
        }
        if (key instanceof JSONPointer) {
            return ((JSONPointer) key).get(this);
        }
        return null;
    }

    @Override
    public void set(String key, Object o) {
        if (key.equals("-")) {
            value.add(valueOf(o));
        } else {
            value.set(Integer.parseInt(key), valueOf(o));
        }
    }

    @Override
    public void add(String key, Object o) {
        if (key.equals("-")) {
            value.add(valueOf(o));
        } else {
            value.add(Integer.parseInt(key), valueOf(o));
        }
    }

    /**
     * Map系
     * @param key
     * @param o
     * @return 
     */
    @Override
    public Object put(String key, Object o) {
        JSONValue v = get(key);
        set(key, o);
        return v;
    }

    @Override
    public JSONValue remove(Object key) {
        if (key instanceof Number) {
            key = key.toString();
        } else if (key instanceof JSONPointer) {
            ((JSONPointer) key).remove(this);
        }
        return value.remove(Integer.parseInt((String) key));
    }

    @Override
    public String toString(JSONFormat format) {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        sb.append(format.crlf);
        for (JSONValue val : value) {
            if (sb.length() > 3) {
                sb.append(",");
                sb.append(format.crlf);
            }
            sb.append(format.tab);
            sb.append(tab(val.toString(format)));
        }
        sb.append(format.crlf);
        sb.append("]");
        return sb.toString();
    }

    @Override
    public boolean equals(Object o) {
        if (o instanceof JSONArray) {
            return value.equals(((JSONArray) o).value);
        } else {
            return false;
        }
    }

    @Override
    public int size() {
        return value.size();
    }

    @Override
    public void clear() {
        value.clear();
    }

    /**
     * 疑似Map
     *
     * @return
     */
    @Override
    public Set<String> keySet() {
        Set<String> s = new HashSet();
        for (int i = 0; i < value.size(); i++) {
            s.add(Integer.toString(i));
        }
        return s;
    }

    @Override
    public boolean isEmpty() {
        return value.isEmpty();
    }

    @Override
    public Iterator<JSONValue> iterator() {
        return value.iterator();
    }

    @Override
    public ValueType getValueType() {
        return ValueType.ARRAY;
    }

    @Override
    public JsonArray asJsonArray() {
        return super.asJsonArray(); //To change body of generated methods, choose Tools | Templates.
    }

    public JsonObject getJsonObject(int i) {
        return (JsonObject) ((JSONCollection) get(i)).toJson();
    }

    public JsonArray getJsonArray(int i) {
        return (JsonArray) ((JSONCollection) get(i)).toJson();
    }

    public JsonNumber getJsonNumber(int i) {
        return (JsonNumber) get(i);
    }

    public JsonString getJsonString(int i) {
        return (JsonString) get(i);
    }

    public <T extends JsonValue> List<T> getValuesAs(Class<T> type) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    public String getString(int i) {
        return (String) get(i).value();
    }

    public String getString(int i, String def) {
        JSONValue val = get(i);
        return (val == null) ? def : (String) val.value();
    }

    public int getInt(int i) {
        return ((JsonNumber) get(i)).intValue();
    }

    public int getInt(int i, int i1) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    public boolean getBoolean(int i) {
        return ((JSONBoolean) get(i)).map();
    }

    public boolean getBoolean(int i, boolean bln) {
        JSONValue val = get(i);
        return val == null ? bln : ((JSONBoolean) val).map();
    }

    public boolean isNull(int i) {
        JSONValue val = get(i);
        return val instanceof JSONNULL;
    }

}
