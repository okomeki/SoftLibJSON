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
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.json.JsonArray;
import javax.json.JsonValue;
import net.siisise.json.JSONFormat;
import net.siisise.json.jsonp.JSONPArray;

/**
 * Listを拡張したJSONArray。
 * 一般のデータを保持してJSONにも変換可能なスタイル。
 * 配列、Listの他ObjectのコンストラクタにもtypeMap可能。
 * JSONP準拠のものはEをJsonValueにするといい。
 * 
 * JsonArray,JsonArrayBuilder,JsonStructure ではない
 * @param <E>
 */
public class JSON2Array<E> extends ArrayList<E> implements JSON2Collection<E> {

    /**
     * Eを参照したいので持っておく型
     * JsonArray用かもしれない
     */
    private final Class<E> def;

    public JSON2Array() {
        def = null;
    }

    protected JSON2Array(Class<E> c) {
        def = c;
    }

    public JSON2Array(Collection<E> vals) {
        super(vals);
        def = null;
    }

    @Override
    public JSON2Value getJSON(String key) {
        return JSON2.valueOf(get(Integer.parseInt(key)));
    }

    @Override
    public void setJSON(String key, JSON2Value obj) {
        E val = def == null ? obj.map() : obj.typeMap(def);
        if (key.equals("-")) {
            add(val);
        } else {
            set(Integer.parseInt(key), val);
        }
    }

    @Override
    public void addJSON(String key, JSON2Value obj) {
        E val = def == null ? obj.map() : obj.typeMap(def);
        if (key.equals("-")) {
            add(val);
        } else {
            add(Integer.parseInt(key), val);
        }
    }

    @Override
    public JSON2Value removeJSON(String key) {
        return JSON2.valueOf(remove(Integer.parseInt(key)));
    }

    @Override
    public JSON2Value putJSON(String key, JSON2Value obj) {
        JSON2Value val = getJSON(key);
        setJSON(key, obj);
        return val;
    }

    /**
     * primitive に変える
     *
     * @param val
     */
    public void addValue(Object val) {
        if (def != null) {
            val = JSON2.valueOf(val).typeMap(def);
        } else {
            val = JSON2.valueMap(val, null);
        }
        add((E) val);
    }

//    JSON2Value getJSON(int index) {
//        return JSON2.valueOf(get(index));
//    }
    static Class<? extends Collection>[] COLL = new Class[]{
        JSON2Array.class, ArrayList.class, HashSet.class, LinkedList.class};
    
    private static Collection typeToList(Class cls) {
        for (Class<? extends Collection> colCls : COLL ) {
            if ( cls.isAssignableFrom(colCls)) {
                try {
                    return colCls.getConstructor().newInstance();
                } catch (NoSuchMethodException | SecurityException | InstantiationException
                        | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
                    Logger.getLogger(JSON2Array.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }
        
        try {
            return (Collection)cls.getConstructor().newInstance();
        } catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException
                | IllegalArgumentException | InvocationTargetException ex) {
            Logger.getLogger(JSON2Array.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
//        throw new UnsupportedOperationException("未サポートな型:" + cls.getTypeName());
    }
    

    @Override
    public <T> T typeMap(Type type) {
        if (type instanceof Class) {
            return (T) classMap((Class) type);
        } else if (type instanceof ParameterizedType) { // List<A>
            ParameterizedType pt = (ParameterizedType) type;
            Type raw = pt.getRawType();
            Collection col = typeToList((Class)raw);
            if ( col != null ) {
                return collectionTypeMap(pt, col);
            }
        }
//        return null;
        throw new UnsupportedOperationException("未サポートな型:" + type.getTypeName());
    }

    private <T> T classMap(Class<T> cls) {
        if (cls == String.class || cls == CharSequence.class) {
            return (T) toString();
        } else if (cls.isAssignableFrom(this.getClass())) {
            return (T) this;
        } else if (!cls.isAssignableFrom(List.class) && cls.isAssignableFrom(JsonArray.class)) { // List を除く
            return (T) toJson();
        } else if (cls.isArray()) { // 配列 要素の型も指定可能, Memberの型ではParameterizedTypeに振り分けられそう?
            Class componentType = cls.getComponentType();
            Object array = (Object) Array.newInstance(componentType, size());

//            j2Stream().map(v -> v.typeMap(componentType)).array);
            int i = 0;
            for (Object val : this) {
                if (val instanceof JSON2Value) {
                    Array.set(array, i++, ((JSON2Value) val).typeMap(componentType));
                } else {
                    Array.set(array, i++, JSON2.valueOf(val).typeMap(componentType));
                }
            }
            return (T) array;
        }
        
        // Collection 要素の型は?
        Collection col = typeToList(cls);
        if ( col != null ) {
            forEach(col::add);
            return (T)col;
        }
        return (T) lcMap(cls);
    }

    private <T> T lcMap(Class<T> cls) {

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
                    JSON2Value v;
                    if (o instanceof JSON2Value) {
                        v = ((JSON2Value) o);
                    } else {
                        v = JSON2.valueOf(o);
                    }
                    params[i] = v.typeMap(pt[i]);
                }
                return (T) c.newInstance(params);
            } catch (UnsupportedOperationException | InstantiationException
                    | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
                Logger.getLogger(JSON2Array.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        throw new UnsupportedOperationException();
    }

    Stream<JSON2Value> j2Stream() {
        return stream().map(v -> (JSON2Value) JSON2.valueOf(v));
    }

    /**
     *
     * @param <T>
     * @param type Generic List&lt;A&gt; のA が取れる
     * @param colCls List,Setの実装class
     * @return
     */
    private <T> T collectionTypeMap(ParameterizedType type, Collection col) {
        // 要素(単体)の型
        Type[] argTypes = type.getActualTypeArguments();
        if (argTypes.length == 0) { // 未検証
            forEach(col::add);
        } else {
            j2Stream().map(v -> v.typeMap(argTypes[0])).forEach(col::add);
        }
        return (T) col;
    }

    @Override
    public JsonArray toJson() {
        if (isEmpty()) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }
        JSONPArray ar = new JSONPArray();
        j2Stream().map(val -> val.toJson()).forEach(ar::add);
        return ar;
    }

    @Override
    public String toString() {
        return toString(NOBR);
    }

    @Override
    public String toString(JSONFormat format) {
        return stream().parallel().map(val -> { return format.crlf + format.tab + tab(JSON2.valueOf(val).toString(format));})
                .collect( Collectors.joining(",", "[", format.crlf +  "]"));
    }

    @Override
    public <T> T map() {
        JSON2Array list = new JSON2Array();
        forEach(list::add);
        return (T) list;
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
        Type cls = clss[0];
        if (clss.length == 1) {
            return typeMap(cls);
        } else {
            Type[] tps = new Type[clss.length];
            int i = 0;
            for (Class c : clss) {
                tps[i++] = c;
            }
            return typeMap(JSON2.parameterizedType(tps));
        }
    }

}
