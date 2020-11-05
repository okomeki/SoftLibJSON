package net.siisise.json2;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonArray;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import net.siisise.json.JSON;
import net.siisise.json.JSONFormat;
import net.siisise.json.jsonp.JSONPArray;

/**
 * Listを拡張したJSONArray.
 * 一般のデータを保持してJSONにも変換可能なスタイル.
 * 配列、Listの他ObjectのコンストラクタにもtypeMap可能
 * @param <E>
 */
public class JSON2Array<E> extends ArrayList<E> implements JSON2Collection<E> {
    
    public JSON2Array() {
    }

    public JSON2Array(Collection<E> vals) {
        super(vals);
    }
    
    @Override
    public JSON2Value getJSON(String key) {
        return JSON2.valueWrap(get(Integer.parseInt(key)));
    }
    
    @Override
    public void setJSON(String key, JSON2Value obj) {
        if (key.equals("-")) {
            add(obj.map());
        } else {
            set(Integer.parseInt(key), obj.map());
        }
    }
    
    @Override
    public void addJSON(String key, JSON2Value obj) {
        if (key.equals("-")) {
            add(obj.map());
        } else {
            add(Integer.parseInt(key), obj.map());
        }
    }
    
    @Override
    public JSON2Value removeJSON(String key) {
        return JSON2.valueWrap(remove(Integer.parseInt(key)));
    }
    
    @Override
    public JSON2Value putJSON(String key, JSON2Value obj) {
        JSON2Value val = getJSON(key);
        setJSON(key, obj);
        return val;
    }
    
//    JSON2Value getJSON(int index) {
//        return JSON2.valueOf(get(index));
//    }
    static Class<? extends Collection>[] COLL = new Class[]{
        JSON2Array.class, ArrayList.class, HashSet.class, LinkedList.class};
    
    @Override
    public <T> T typeMap(Type type) {
        if (type instanceof Class) {
            Class cls = (Class) type;
            if (cls == String.class) {
                return (T) toString();
            } else if (cls.isAssignableFrom(this.getClass())) {
                return (T) this;
            } else if (cls.isAssignableFrom(List.class)) {
                // JsonArrayからList除外
            } else if (cls.isAssignableFrom(JsonArray.class)) { // List を除く
                return (T) toJson();
            } else if (cls.isArray()) { // 配列 要素の型も指定可能, Memberの型ではParameterizedTypeに振り分けられそう?
                Class componentType = cls.getComponentType();
                Object array = (Object) Array.newInstance(componentType, size());

                int i = 0;
                for (Object val : this) {
                    if ( val instanceof JSON2Value ) {
                        Array.set(array, i++, ((JSON2Value)val).typeMap(componentType));
                    } else {
                        Array.set(array, i++, JSON.valueOf(val).typeMap(componentType));
                    }
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
    
    private <T> T lcMap(Class... clss) {
        Class<T> cls = clss[0];

        // Collection 要素の型は?
        for (Class<? extends Collection> colCls : COLL) {
            if (cls.isAssignableFrom(colCls)) {
                return collectionMap(colCls, clss);
            }
        }
        // ToDo: コンストラクタに突っ込む.
        int len = size();
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
                    Object o = get(i);
                    if ( o instanceof JSON2Value ) {
                        params[i] = ((JSON2Value)o).typeMap(pt[i]);
                    } else {
                        params[i] = JSON.valueOf(o).typeMap(pt[i]);
                    }
                }
                return (T) c.newInstance(params);
            } catch (UnsupportedOperationException | InstantiationException
                    | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
                Logger.getLogger(JSON2Array.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        throw new UnsupportedOperationException();
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

            forEach(o -> {
                if ( o instanceof JSON2Value ) {
                    col.add(((JSON2Value)o).typeMap(argTypes[0]));
                } else {
                    col.add(JSON.valueOf(o).typeMap(argTypes[0]));
                }
            });
            return (T) col;
        } catch (NoSuchMethodException | SecurityException | InstantiationException
                | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            Logger.getLogger(JSON2Array.class.getName()).log(Level.SEVERE, null, ex);
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
    private <T> T collectionMap(Class<? extends Collection> colCls, Class... clss) {
        Collection col;

        try {
            col = colCls.getConstructor().newInstance();

            if (clss.length > 1) {
                Class[] clb = new Class[clss.length - 1];
                System.arraycopy(clss, 1, clb, 0, clb.length);

                forEach(o -> {
                    if (o instanceof JSON2Collection) {
                        col.add(((JSON2Collection)o).map(clb));
                    } else {
                        col.add(JSON.valueOf(o).typeMap(clss[1]));
                    }
                });
            } else {
                forEach(o -> {
                    col.add(o);
                });
            }
            return (T) col;
        } catch (InstantiationException | IllegalAccessException | IllegalArgumentException
                | InvocationTargetException | NoSuchMethodException | SecurityException ex) {
            Logger.getLogger(JSON2Array.class.getName()).log(Level.SEVERE, null, ex);
        }
        throw new UnsupportedOperationException();
    }

    @Override
    public JsonStructure toJson() {
        if ( isEmpty() ) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }
        JSONPArray ar = new JSONPArray();
        forEach(val -> {
            ar.add(JSON.valueOf(val).toJson());
        });
        return ar;
    }

    @Override
    public String toString() {
        return toString(TAB);
    }

    @Override
    public String toString(JSONFormat format) {
        StringBuilder sb = new StringBuilder();
        sb.append("[");
        sb.append(format.crlf);
        for ( Object val : this ) {
            sb.append(format.tab);
            if ( val instanceof JSON2Value ) {
                sb.append(tab(((JSON2Value)val).toString(format)));
            } else {
                sb.append(tab(JSON.valueOf(val).toString(format)));
            }
            sb.append(",");
            sb.append(format.crlf);
        }
        if ( !isEmpty() ) {
            sb.replace(sb.length() - format.crlf.length() - 1, sb.length() - format.crlf.length(), "");
        }
//        sb.append(format.crlf);
        sb.append("]");
        return sb.toString();
    }

    @Override
    public <T> T map() {
        JSON2Array list = new JSON2Array();
        forEach(val -> {
            list.add(val);
        });
        return (T)list;
    }

    /**
     * List または　配列に変換する.
     *
     * @deprecated #typeMap(Type)
     * @param <T>
     * @param clss Collection または配列、JSONArray、それ以外は要素の型として扱う
     * @return 配列をObject に入れる罠
     */
    @Override
    public <T> T map(Class... clss) {
        Class<T> cls = clss[0];
        if (cls == String.class) {
            return typeMap(cls);
        } else if (cls.isAssignableFrom(this.getClass()) && clss.length == 1) {
            return typeMap(cls);
        } else if (cls.isAssignableFrom(List.class)) {
            return lcMap(clss);
        } else if (cls.isAssignableFrom(JsonArray.class)) { // List を除く
            return typeMap(cls);
        } else if (cls.isArray()) { // 配列 要素の型も指定可能
            return typeMap(cls);
        }
        return lcMap(clss);
    }

}
