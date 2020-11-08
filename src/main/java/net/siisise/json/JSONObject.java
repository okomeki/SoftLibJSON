package net.siisise.json;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonObject;
import javax.json.JsonValue;
import net.siisise.json.jsonp.JSONPObject;
import net.siisise.json.pointer.JSONPointer;
import net.siisise.json2.JSON2Object;

/**
 * JSONのObject
 * JavaのMapに名前順ソートを足したもの。
 * public field のみ対応でいい?
 *
 * @see javax.json.JsonObject
 */
public class JSONObject extends JSONValue<Map<String, JSONValue>> implements JSONCollection<String, Map<String, JSONValue>> {

    private final List<String> names;

    public JSONObject() {
        this.names = new ArrayList<>();
        value = new HashMap<>();
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
        Map<String, JSONValue> ret = new HashMap<>();
        value.keySet().forEach(key -> {
            ret.put(key, value.get(key));
        });
        return ret;
    }

    @Override
    public Map<String, Object> map() {
        Map<String, Object> ret = new JSON2Object<>();
        value.keySet().forEach(key -> {
            ret.put(key, value.get(key).map());
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
        if (type instanceof Class) {
            return classMap((Class)type);
        } else if (type instanceof ParameterizedType) { // まだ Map<A,B> だけ
            ParameterizedType pt = (ParameterizedType) type;
            Type raw = pt.getRawType();
            if ((raw instanceof Class) && (Map.class.isAssignableFrom(((Class) raw)))) {
                Type[] args = pt.getActualTypeArguments();
                Map map = new HashMap();
                value.keySet().forEach(key -> {
                    JSONString jsonKey = new JSONString(key);
                    map.put(jsonKey.typeMap(args[0]), value.get(key).typeMap(args[1]));
                });
                return (T) map;
            }
        }
        throw new UnsupportedOperationException("未実装...:" + type.getClass());
    }
    
    private <T> T classMap(Class cls) {
        if (cls == String.class) {
            return (T) toString();
        } else if (cls.isAssignableFrom(this.getClass())) {
            return (T) this;
        } else if (Map.class.isAssignableFrom(cls)) { // GenericのないMapの場合単純なJava型に変換する
            Map map = new JSON2Object();
            value.keySet().forEach(key -> {
                JSONValue val = value.get(key);
                map.put(key, val.map());
            });
            return (T) map;
        } else if (cls.isAssignableFrom(JsonObject.class)) {
            return (T)toJson();
        }
        return (T) map(cls);
        
    }

    /**
     * Collection 文字列、JSONObject, JsonObject, Map, Java Object にマップする
     *
     * @deprecated #typeMap(Type) へ移行する
     * @param <T>
     * @param clss
     * @return
     */
    @Override
    public <T> T map(Class... clss) {
        Class<T> cls = clss[0];
        if (cls == String.class) {
            return typeMap(cls);
        } else if (cls.isAssignableFrom(this.getClass())) {
            return (T) this;
        } else if (Map.class.isAssignableFrom(cls)) { // まだ
            Map map = new HashMap();
            value.keySet().forEach(key -> {
                JSONValue val = value.get(key);
                if (clss.length == 3) {
                    map.put(key, val.typeMap(JSONMap.replaces, clss[2]));
                } else {
                    map.put(key, val.map());
                }
            });
            return (T) map;
        } else if (cls.isAssignableFrom(JsonObject.class)) { // なし
            return (T) toJson();
        }

        try {
            T obj = cls.getConstructor().newInstance();
            for (String name : names) {
//                try {
                Field field = null; // = cls.getField(name);

                Class c = cls;
                while (c != null && field == null) {
                    try {
                        field = c.getDeclaredField(name);
                    } catch (NoSuchFieldException e) {
                        c = c.getSuperclass();
                    }
                }

//                    Field field = cls.getDeclaredField(name);
                Type gtype = field.getGenericType();
//                    System.out.println("obj.generictype.class:" + gt.getClass().getName());
//                    System.out.println("obj.generictype.typename:" + gt.getTypeName());
                field.set(obj, value.get(name).typeMap(JSONMap.replaces, gtype));
//                } catch (NoSuchFieldException ex) { // fieldがないときは捨てるかサブクラスを探すか
//                    Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
//                }
            }
            return obj;
        } catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
            throw new java.lang.UnsupportedOperationException(ex);
        }
//        throw new java.lang.UnsupportedOperationException("えらー");
    }

    /**
     *
     * @param <T>
     * @param value
     * @param clss
     * @return
     */
    /*
    static <T> T valueMap(Map<String,Object> value, Class... clss) {
        Class<T> cls = clss[0];
        if (cls == String.class) {
            return (T) JSON.valueOf(value).toString();
        } else if (cls.isAssignableFrom(this.getClass())) {
            return (T) this;
        } else if (Map.class.isAssignableFrom(cls)) { // まだ
            Map map = new HashMap();
            value.keySet().forEach(key -> {
                JSONValue val = value.get(key);
                if (clss.length == 3) {
                    map.put(key, val.map(JSONMap.replaces, clss[2]));
                } else {
                    map.put(key, val.map());
                }
            });
            return (T) map;
        } else if (cls.isAssignableFrom(JsonObject.class)) { // なし
            return (T) toJson();
        }

        try {
            T obj = cls.getConstructor().newInstance();
            for (String name : names) {
//                try {
                    Field field = null; // = cls.getField(name);

                    Class c = cls;
                    while ( c != null && field == null ) {
                        try {
                            field = c.getDeclaredField(name);
                        } catch (NoSuchFieldException e) {
                            c = c.getSuperclass();
                        }
                    }
                    
                    
//                    Field field = cls.getDeclaredField(name);
                    Type gtype = field.getGenericType();
//                    System.out.println("obj.generictype.class:" + gt.getClass().getName());
//                    System.out.println("obj.generictype.typename:" + gt.getTypeName());
                    field.set(obj, value.get(name).typeMap(JSONMap.replaces, gtype));
//                } catch (NoSuchFieldException ex) { // fieldがないときは捨てるかサブクラスを探すか
//                    Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
//                }
            }
            return obj;
        } catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
            throw new java.lang.UnsupportedOperationException(ex);
        }
//        throw new java.lang.UnsupportedOperationException("えらー");
    }
     */
    @Override
    public JsonObject toJson() {
        if (value.isEmpty()) {
            return JsonValue.EMPTY_JSON_OBJECT;
        } else {
            JSONPObject obj = new JSONPObject();
            names.forEach(name -> {
                obj.put(name, get(name).toJson());
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
    public JSONValue put(String key, Object value) {
        JSONValue val = this.value.put(key, JSON.valueOf(value));
        if (!names.contains(key)) {
            names.add(key);
        }
        return val;
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
        JSONValue val = value.put(key, obj);
        if (!names.contains(key)) {
            names.add(key);
        }
        return val;
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
        if (names.contains(key)) {
            names.remove(key);
            return value.remove(key);
        } else {
            throw new java.lang.UnsupportedOperationException();
        }
    }

    @Override
    public JSONValue remove(String key) {
        if (names.contains(key)) {
            names.remove(key);
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
     * Java Objectの公開フィールドを取得してJSONObjectに変換する toJSON() がある場合には対応する
     *
     * @param obj
     * @return
     */
    public static JSONValue convObject(Object obj) {
        Class<? extends Object> cls = obj.getClass();
        try {
            Method toj = cls.getMethod("toJSON");
            String json = (String) toj.invoke(obj);
            return JSON.parse(json);
        } catch (NoSuchMethodException ex) {
            // 特にないので標準の変換へ
        } catch (SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
        }
        Field[] fields = cls.getFields();
        JSONObject jsonobj = new JSONObject();

        for (Field field : fields) {
            try {
                jsonobj.setJSON(field.getName(), JSON.valueOf(field.get(obj)));
            } catch (IllegalArgumentException | IllegalAccessException ex) {
                Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return jsonobj;
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
        names.clear();
        value.clear();
    }

    @Override
    public Iterator<JSONValue> iterator() {
        return value.values().iterator();
    }

}
