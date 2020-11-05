package net.siisise.json2;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonObject;
import javax.json.JsonValue;
import net.siisise.json.JSON;
import net.siisise.json.JSONFormat;
import net.siisise.json.jsonp.JSONPObject;

/**
 * JavaではMapに相当する中間型。
 * typeMapによりObjectとも対応する。
 * 
 * @param <V> JSONで可能な型、または自由?
 */
public class JSON2Object<V> extends HashMap<String, V> implements JSON2Collection<V> {

    private List<String> names = new ArrayList();
    
    public JSON2Object() {
    }
    
    public JSON2Object(Map map) {
        map.keySet().forEach(key -> {
            V o = (V) JSON2.valueMap(map.get(key));
            if ( key instanceof String) {
                put((String)key, o);
            } else {
                put(key.toString(), o);
            }
        });
    }
    
    @Override
    public JSON2Value getJSON(String key) {
        return JSON2.valueWrap(get(key));
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
        return JSON2.valueWrap(put(key, obj.map()));
    }

    @Override
    public JSON2Value removeJSON(String key) {
        if(names.contains(key)) {
            names.remove(key);
            return JSON2.valueWrap(remove(key));
        }
        return null;
    }
    
    public V remove(String key) {
        names.remove(key);
        return super.remove(key);
    }
    
    @Override
    public V put(String key, V val) {
        if ( !names.contains(key)) {
            names.add(key);
        }
        return super.put(key, val);
    }
    
    /**
     *
     * @param
     * @return
     */
    @Override
    public HashMap<String,V> map() {
        return this;
    }

    @Override
    public <T> T typeMap(Type type) {
        if (type instanceof ParameterizedType) {
                ParameterizedType pt = (ParameterizedType)type;
                Type raw = pt.getRawType();
                if ( (raw instanceof Class) && (Map.class.isAssignableFrom(((Class)raw)))) {
                    Type[] args = pt.getActualTypeArguments();
                    Map map = new HashMap();
                    keySet().forEach(key -> {
                        JSON2String jsonKey = new JSON2String(key);
                        map.put(jsonKey.typeMap(args[0]), JSON.valueOf(get(key)).typeMap(args[1]));
                    });
                    return (T)map;
                }

        } else if ( type instanceof Class ) {
            Class cls = (Class)type;
            if (cls == String.class) {
                return (T) toString();
            } else if (cls.isAssignableFrom(this.getClass())) {
                return (T) this;
            } else if ( Map.class.isAssignableFrom(cls)) { // 表面だけ軽い複製 ToDO: 全部複製?
                JSON2Object map = new JSON2Object();
                keySet().forEach(key -> {
                    V val = get(key);
                    map.put(key, val);
                });
                return (T)map;
            }
            return lcMap((Class)type);
        }
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
    @Override
    public <T> T map(Class... clss) {
        Class<T> cls = clss[0];
        if ( cls == String.class) {
            return (T) toString();
        } else if (cls.isAssignableFrom(this.getClass())) {
            return (T) this;
        } else if (Map.class.isAssignableFrom(cls)) { // まだ
            Map map = new HashMap();
            keySet().forEach(key -> {
                V val = get(key);
                if (clss.length == 3) {
                    map.put(key, JSON2.valueWrap(val).typeMap(clss[2]));
                } else {
                    map.put(key, val);
                }
            });
            return (T) map;
        }
        return lcMap(clss);
    }

    <T> T lcMap(Class... clss) {
        Class<T> cls = clss[0];
        if (cls.isAssignableFrom(JsonObject.class)) { // なし
            return (T) toJson();
        }

        try {
            T obj = cls.getConstructor().newInstance();
            for (String name : keySet()) {
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
                    field.set(obj, JSON2.valueWrap(get(name)).typeMap(gtype));
//                } catch (NoSuchFieldException ex) { // fieldがないときは捨てるかサブクラスを探すか
//                    Logger.getLogger(JSONObject.class.getName()).log(Level.SEVERE, null, ex);
//                }
            }
            return obj;
        } catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            Logger.getLogger(JSON2Object.class.getName()).log(Level.SEVERE, null, ex);
            throw new java.lang.UnsupportedOperationException(ex);
        }
    }

    @Override
    public JsonValue toJson() {
        if (isEmpty()) {
            return JsonValue.EMPTY_JSON_OBJECT;
        } else {
            JSONPObject obj = new JSONPObject();
            keySet().forEach(name -> {
                obj.put(name, JSON2.valueWrap(get(name)).toJson());
            });
            return obj;
        }
    }

    public String toString() {
        return toString(TAB);
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
            Object val = get(name);
            if (val instanceof JSON2) {
                sb.append(tab(((JSON2) val).toString(format)));
            } else {
                sb.append(tab(JSON2.valueWrap(val).toString(format)));
            }
        }
        sb.append(format.crlf);
        sb.append("}");
        return sb.toString();
    }

    /**
     * 
     * @param obj
     * @return 
     */
    static Object convObject(Object obj) {
        Class<? extends Object> cls = obj.getClass();
        try {
            Method toj = cls.getMethod("toJSON");
            String json = (String) toj.invoke(obj);
            return JSON2.parse(json);
        } catch (NoSuchMethodException ex) {
            // 特にないので標準の変換へ
        } catch (SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            Logger.getLogger(JSON2Object.class.getName()).log(Level.SEVERE, null, ex);
        }
        Field[] fields = cls.getFields();
        JSON2Object jsonobj = new JSON2Object();

        for (Field field : fields) {
            try {
                jsonobj.put(field.getName(), JSON2.valueWrap(field.get(obj)));
            } catch (IllegalArgumentException | IllegalAccessException ex) {
                Logger.getLogger(JSON2Object.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return jsonobj;
    }

}
