package net.siisise.json;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collector;
import java.util.stream.Collectors;
import javax.json.JsonArray;
import javax.json.JsonNumber;
import javax.json.JsonValue;
import net.siisise.json.jsonp.JSONPArray;
import net.siisise.json.pointer.JSONPointer;
import net.siisise.json2.JSON2;
import net.siisise.json2.JSON2Array;

/**
 * 配列またはList。
 * 数字をKeyとした疑似Mapとして操作できる
 *
 * @see javax.json.JsonArray
 */
public class JSONArray extends JSONValue<List<JSONValue>> implements JSONCollection<String, List<JSONValue>> {

    public JSONArray() {
        value = new ArrayList<>();
    }

    public JSONArray(Collection list) {
        //value = new ArrayList<>();
        value = (List<JSONValue>) list.stream().map(src -> JSON.valueOf(src) ).collect(Collectors.toList());
    }

    @Override
    public List<JSONValue> value() {
        return new ArrayList(value);
    }

    static Collector<JsonValue, ?, JSONPArray> toJSONPArray() {
        return Collector.of(
            JSONPArray::new,
            List::add,
            (ls, l2) -> {
                ls.addAll(l2);
                return ls;
            }
            );
    }

    /**
     * 要素に変換したもの
     *
     * @return
     */
    @Override
    public List map() {
        return value.stream().map(v -> v.map()).collect(JSON2.toJSON2PrimArray());
    }

    static Class<? extends Collection>[] COLL = new Class[]{
        JSON2Array.class, ArrayList.class, HashSet.class, LinkedList.class};

    /**
     * 型情報からObject型を推測しながらList または配列にマッピングする.
     *
     * @param <T> 要素型
     * @param type class,型情報
     * @return Listまたは配列
     */
    @Override
    public <T> T typeMap(Type type) {
        if (type instanceof Class) {
            Class cls = (Class) type;
            if (cls == String.class || cls == CharSequence.class ) {
                return (T) toString();
            } else if (cls.isAssignableFrom(this.getClass())) {
                return (T) this;
            } else if (cls.isAssignableFrom(List.class)) {
                // JsonArrayからList除外
            } else if (cls.isAssignableFrom(JsonArray.class)) { // List を除く
                return (T) toJson();
            } else if (cls.isArray()) { // 配列 要素の型も指定可能, Memberの型ではParameterizedTypeに振り分けられそう?
                Class componentType = cls.getComponentType();
                Object array = (Object) Array.newInstance(componentType, value.size());

                int i = 0;
                for (JSONValue val : value) {
                    Array.set(array, i++, val.typeMap(componentType));
                }
                return (T) array;
            }
            return (T) lcMap(cls);
        } else if (type instanceof ParameterizedType) { // List<A>
            ParameterizedType pt = (ParameterizedType) type;
            Type raw = pt.getRawType();
            Class rawClass = (Class) raw;
            for (Class<? extends Collection> colCls : COLL) {
                if (rawClass.isAssignableFrom(colCls)) {
                    return collectionTypeMap(pt, colCls);
                }
            }
        }

        throw new UnsupportedOperationException("未サポートな型:" + type.getTypeName());
    }
    
    private <T> T lcMap(Class<T> cls) {

        // Collection 要素の型は?
        for (Class<? extends Collection> colCls : COLL) {
            if (cls.isAssignableFrom(colCls)) {
                return collectionMap(colCls, cls);
            }
        }
        // ToDo: コンストラクタに突っ込む.
        int len = value.size();
        Constructor[] cnss = cls.getConstructors();
//        List<Constructor> cons = new ArrayList<>();

        for (Constructor c : cnss) {
            if (c.getParameterCount() != len) {
                continue;
            }

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
                for (int i = 0; i < pt.length; i++) {
                    params[i] = get(i).typeMap(pt[i]);
                }
                return (T) c.newInstance(params);
            } catch (UnsupportedOperationException | InstantiationException
                    | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
                Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        throw new UnsupportedOperationException();
    }

    @Override
    public JsonArray toJson() {
        if (value.isEmpty()) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }
        return value.parallelStream().map(val -> val.toJson()).collect(toJSONPArray());
    }

    /**
     * 
     * @param <T>
     * @param type Generic List&lt;A&gt; のA が取れる
     * @param colCls List,Setの実装class
     * @return 
     */
    private <T> T collectionTypeMap(ParameterizedType type, Class<? extends Collection> colCls) {
        Collection col;

        try {
            col = colCls.getConstructor().newInstance();

            // 要素(単体)の型
            Type[] argTypes = type.getActualTypeArguments();
//            Class[] clb = new Class[argTypes.length];

            value.parallelStream().map(o -> o.typeMap(argTypes[0])).forEach(col::add);
            return (T) col;
        } catch (NoSuchMethodException | SecurityException | InstantiationException
                | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
        }
        throw new UnsupportedOperationException();
    }

    /**
     *
     * @deprecated #collectionTypeMap(ParameterizedMap,Class<? extends Collection>)
     * @param <T>
     * @param colCls
     * @param clss
     * @return
     */
    private <T> T collectionMap(Class<? extends Collection> colCls, Class<T> cls) {
        Collection col;

        try {
            col = colCls.getConstructor().newInstance();

            value.forEach(col::add);
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

        if (contentType == JSONValue.class) {
            return value.toArray(a);
        }

        return value.parallelStream().map(v -> v.typeMap(contentType)).collect(Collectors.toList()).toArray(a);
    }

    public void add(Object o) {
        value.add(JSON.valueOf(o));
    }

    public void add(JSONValue o) {
        value.add(JSON.valueOf(o));
    }

    public JSONValue get(int index) {
        return value.get(index);
    }

    @Override
    public JSONValue get(String key) {
        return value.get(Integer.parseInt(key));
    }

    @Override
    public JSONValue getJSON(String key) {
        return value.get(Integer.parseInt(key));
    }

    @Override
    public JSONValue getJSON(JSONPointer point) {
        return point.get(this);
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

    /**
     * Collection要員だったもの
     *
     * @param key
     * @param o
     */
    @Override
    public void set(String key, Object o) {
        if (key.equals("-")) {
            value.add(JSON.valueOf(o));
        } else {
            value.set(Integer.parseInt(key), JSON.valueOf(o));
        }
    }

    @Override
    public void setJSON(String key, JSONValue obj) {
        if (key.equals("-")) {
            value.add(obj);
        } else {
            value.set(Integer.parseInt(key), obj);
        }
    }

    /**
     * Collection要員だったもの
     *
     * @param key
     * @param o
     */
    public void add(String key, Object o) {
        if (key.equals("-")) {
            value.add(JSON.valueOf(o));
        } else {
            value.add(Integer.parseInt(key), JSON.valueOf(o));
        }
    }

    @Override
    public void addJSON(String key, JSONValue obj) {
        if (key.equals("-")) {
            value.add(obj);
        } else {
            value.add(Integer.parseInt(key), obj);
        }
    }

    /**
     * Map系
     * Collection要員だったもの
     *
     * @param key
     * @param o
     * @return
     */
    public JSONValue put(String key, Object o) {
        JSONValue v = get(key);
        set(key, o);
        return v;
    }

    /**
     * Map系
     *
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
        return value.remove(Integer.parseInt(key));
    }

    /**
     *
     * Collection要員だったもの
     *
     * @param key
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

    @Override
    public JSONValue removeJSON(String key) {
        return value.remove(Integer.parseInt(key));
    }

    @Override
    public String toString(JSONFormat format) {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        sb.append(format.crlf);
        for (JSONValue val : value) {
            sb.append(format.tab);
            sb.append(tab(val.toString(format)));
            sb.append(",");
            sb.append(format.crlf);
        }
        if ( !value.isEmpty() ) {
            sb.replace(sb.length() - format.crlf.length() - 1, sb.length() - format.crlf.length(), "");
        }
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
     * Collection要員だったもの
     *
     * @return
     */
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

    @Override
    public boolean isEmpty() {
        return value.isEmpty();
    }

    @Override
    public Iterator<JSONValue> iterator() {
        return value.iterator();
    }

    public String getString(int i) {
        return (String) get(i).value();
    }

    public String getString(int i, String def) {
        JSONValue val = get(i);
        return (val == null) ? def : (String) val.value();
    }

    public int getInt(int index) {
        return ((JsonNumber) get(index)).intValue();
    }

    public int getInt(int index, int def) {
        JSONValue val = get(index);
        return (val == null) ? def : (int) val.typeMap(Integer.TYPE);
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
