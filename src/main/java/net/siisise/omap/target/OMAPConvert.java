package net.siisise.omap.target;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Collection;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;
import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONNULL;
import net.siisise.json.JSONString;
import net.siisise.json.jsonp.JSONPArray;
import net.siisise.json2.JSON2;
import net.siisise.json2.JSON2Array;
import net.siisise.json2.JSON2Boolean;
import net.siisise.json2.JSON2NULL;
import net.siisise.json2.JSON2Object;
import net.siisise.json2.JSON2String;
import net.siisise.json2.JSON2Value;
import net.siisise.omap.OMAP;

/**
 * String Integer, 継承関係型 JSON2NULL など継承、未解決型への変換.
 * あとで分ける
 * @param <T>
 */
public class OMAPConvert<T> extends OBJConvert<T> {
    
    Type type;
    
    public OMAPConvert(Type cls) {
        this.type = cls;
    }

    @Override
    public Type targetClass() {
        return type;
    }

    @Override
    public Object nullValue() {
        if ( type instanceof ParameterizedType ) {
            type = ((ParameterizedType)type).getRawType();
        }

        if ( type instanceof Class ) {
            Class cls = (Class)type;
            if ( cls.isAssignableFrom(JSON2NULL.class)) {
                return JSON2NULL.NULL;
            } if ( cls.isAssignableFrom(JSONNULL.class)) {
                return JSONNULL.NULL;
            }
        }
        // String Integer Boolean などもnull
        return null;
    }

    @Override
    public Object booleanValue(Boolean bool) {
        if ( type instanceof Class ) {
            Class cls = (Class) type;
            if ( cls.isAssignableFrom(Boolean.class) || cls == Boolean.TYPE ) {
                return (Boolean)bool;
            } else if ( cls == String.class || cls == CharSequence.class ) {
                return Boolean.toString(bool);
            } else if ( cls.isAssignableFrom(JSON2Boolean.class) ) {
                return (bool ? JSON2Boolean.TRUE : JSON2Boolean.FALSE);
            } else if ( cls.isAssignableFrom(JSONBoolean.class) ) {
                return (bool ? JSONBoolean.TRUE : JSONBoolean.FALSE);
            } else if ( cls.isAssignableFrom(Integer.class) ) {
                return Integer.valueOf( bool ? 1 : 0 );
            } else if ( cls.isAssignableFrom(Byte.class) ) {
                return Byte.valueOf( bool ? (byte)1 : (byte)0 );
            } else if ( cls.isAssignableFrom(Short.class) ) {
                return Short.valueOf( bool ? (short)1 : (short)0 );
            } else if ( cls.isAssignableFrom(Long.class) ) {
                return Long.valueOf( bool ? 1 : 0 );
            }
        }
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Object numberValue(Number number) {
        if ( !(type instanceof Class) ) {
            throw new UnsupportedOperationException("まだ");
        }
        Class cls = (Class)type;
        
        if ( cls.isInstance(number)) {
            return number;
        }

        if (cls == Integer.TYPE || cls == Integer.class) {
            return Integer.valueOf(number.intValue());
        } else if (cls == Long.TYPE || cls == Long.class) {
            return Long.valueOf(number.longValue());
        } else if (cls == Short.TYPE || cls == Short.class) {
            return Short.valueOf(number.shortValue());
        } else if (cls == Character.TYPE || cls == Character.class) {
            return Character.valueOf((char)number.intValue());
        } else if (cls == Byte.TYPE || cls == Byte.class) {
            return Byte.valueOf(number.byteValue());
        } else if (cls == Float.TYPE || cls == Float.class) {
            return Float.valueOf(number.floatValue());
        } else if (cls == Double.TYPE || cls == Double.class) {
            return Double.valueOf(number.doubleValue());
        } else if (cls == BigInteger.class) {
            return new BigInteger(number.toString());
        } else if (cls == BigDecimal.class) {
            return new BigDecimal(number.toString());
        } else if ( cls == String.class || cls == CharSequence.class ) {
            return number.toString();
        }

        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Object stringValue(CharSequence value) {
        if (value == null) {
            return nullValue();
        }
        Class ocls = value.getClass();
        String val;
        if (ocls != String.class ) {
            val = value.toString();
        } else if (type == StringBuilder.class ) {
            return new StringBuilder(value);
        } else if (type == StringBuffer.class ) {
            return new StringBuffer(value);
        } else {
            val = (String)value;
        }
        if ( type instanceof Class ) {
            if ( type == ocls || type == CharSequence.class ) {
                return value;
            }
            if ( type == String.class ) {
                return val;
            }
            Class cls = (Class)type;
            if ( cls.isAssignableFrom(JSON2String.class) ) {
                return new JSON2String(val);
            }
            if ( cls.isAssignableFrom(JSONString.class) ) {
                return new JSONString(val);
            }
            if ( type == UUID.class ) {
                return UUID.fromString(val);
            }
//            if ( type == Date.class ) {
//                return new JSONDateM().replace(new JSON2String(val), null);
//            }
            // 任意の型になるかもしれない注意
            try {
                Constructor c = ((Class)type).getConstructor(value.getClass());
                return c.newInstance(value);
            } catch (NoSuchMethodException | SecurityException | InstantiationException
                    | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
                Logger.getLogger(OMAPConvert.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return value;
    }

    /**
     * まだJSON2Array側にあるかもしれない
     * @param list
     * @return 
     */
    @Override
    public Object listValue(Collection list) {
        if (type instanceof Class) {
            Class cls = (Class) type;
            return listClassCast(list, (Class)type);
        } else if ( type instanceof ParameterizedType ) {
            ParameterizedType pt = (ParameterizedType) type;
            Type raw = pt.getRawType();
            Collection col = typeToList((Class)raw);
            if ( col != null ) {
                return listCollectionTypeMap(list, pt, col);
            }

        }
        
        throw new UnsupportedOperationException("未サポートな型:" + type.getTypeName());
    }

    public static <I,T> T listClassCast(Collection<I> src, Class<T> cls) {
        if (cls == String.class || cls == CharSequence.class) {
            return (T) src.toString();
        } else if (cls.isAssignableFrom(src.getClass())) {
            return (T) src;
        } else if (!cls.isAssignableFrom(List.class) && cls.isAssignableFrom(JsonArray.class)) { // List を除く
            if ( src.isEmpty() ) {
                return (T)JsonValue.EMPTY_JSON_ARRAY;
            }
            return (T) src.stream().collect(JSONPArray.collector());
        } else if (cls.isArray()) { // 配列 要素の型も指定可能, Memberの型ではParameterizedTypeに振り分けられそう?
            Class componentType = cls.getComponentType();
            Object array = (Object) Array.newInstance(componentType, src.size());

//            j2Stream().map(v -> v.typeMap(componentType)).array);
            int i = 0;
            for (Object val : src) {
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
            src.forEach(col::add);
            return (T)col;
        }
        List list;
        if ( src instanceof List ) {
            list = (List)src;
        } else {
            list = new ArrayList(src);
        }
        return (T) listLcCast(list, cls);
    }

    /**
     *
     * @param <T>
     * @param type Generic List&lt;A&gt; のA が取れる
     * @param colCls List,Setの実装class
     * @return
     */
    private <T,M> T listCollectionTypeMap(Collection<M> list, ParameterizedType type, Collection col) {
        // 要素(単体)の型
        Type[] argTypes = type.getActualTypeArguments();
        if (argTypes.length == 0) { // 未検証
            list.forEach(col::add);
        } else {
            list.stream().map(m -> JSON2.valueOf(m).typeMap(argTypes[0])).forEach(col::add);
        }
        return (T) col;
    }

    private static <T> T listLcCast(List array, Class<T> cls) {

        // ToDo: コンストラクタに突っ込む.
        int len = array.size();
        Constructor[] cnss = cls.getConstructors();

        for (Constructor c : cnss) {
            if (c.getParameterCount() != len) {
                continue;
            }

            Type[] pt = c.getGenericParameterTypes();
            Object[] params = new Object[pt.length];
//            cons.add(c);
//        }
//        if ( cons.size() == 1 ) { // 対象っぽいのがあれば
//            Constructor c = cons.get(0);
            try {
                for (int i = 0; i < pt.length; i++) {
                    Object o = array.get(i);
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
                Logger.getLogger(OMAPConvert.class.getName()).log(Level.SEVERE, null, ex);
            }
        }

        throw new UnsupportedOperationException();
    }

    /**
     * MapからMapまたはObjectになるのかもしれない
     * @param map
     * @return 
     */
    @Override
    public Object mapValue(Map map) {
        if (type instanceof Class) {
            return mapClassCast(map, (Class)type);
        } else if ( type instanceof ParameterizedType ) {
            return mapParameterizedCast(map, (ParameterizedType)type);
        }
        throw new UnsupportedOperationException("まだない");
    }

    /**
     * @param type
     * @return
     */
    private <K,V> Map mapParameterizedCast(Map<K,V> src, ParameterizedType type) {
        Type raw = type.getRawType();
        if ((raw instanceof Class) && (Map.class.isAssignableFrom(((Class) raw)))) {
            Type[] args = type.getActualTypeArguments();
            Map map = typeToMap((Class) raw);
            src.entrySet().forEach(es -> {
//                JSON2Value jsonKey = JSON2.valueOf(es.getKey());
                Object tkey = OMAP.valueOf(es.getKey(), args[0]);
//                JSON2Value jsonVal =  JSON2.valueOf(es.getValue());
                Object tval = OMAP.valueOf(es.getValue(), args[1]);
                map.put(tkey, tval);
            });
            return map;
        }
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private <K,V> Object mapClassCast(Map<K,V> obj, Class cls) {
        if (cls == String.class || cls == CharSequence.class) {
            return obj.toString();
        } else if (cls.isAssignableFrom(this.getClass())) {
            return obj;
        } else if (Map.class.isAssignableFrom(cls)) { // 表面だけ軽い複製 ToDO: 全部複製?
            Map<K,V> map = typeToMap(cls);
            obj.entrySet().forEach(es -> {
                map.put(es.getKey(), es.getValue());
            });
            return map;
        }
        if (cls.isAssignableFrom(JsonObject.class)) { // なし
            return OMAP.typeMap(obj, JsonValue.class);
        }
        return mapLc(obj, cls);
    }

    private <K,V,T> T mapLc(Map<K,V> src, Class<T> cls) {
        try {
            T obj = cls.getConstructor().newInstance();
            for (Map.Entry<K,V> es : src.entrySet()) {
                Field field = null;

                Class c = cls;
                while (c != null && field == null) {
                    try {
                        field = c.getDeclaredField(es.getKey().toString());
                        // field.setAccessible(true);
                    } catch (NoSuchFieldException e) {
                        c = c.getSuperclass();
                    }
                }
                field.set(obj, OMAP.valueOf((Object)es.getValue(), field.getGenericType()));
            }
            return obj;
        } catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            Logger.getLogger(OMAPConvert.class.getName()).log(Level.SEVERE, null, ex);
            throw new java.lang.UnsupportedOperationException(ex);
        }
    }

    static Class[] MAPS = {HashMap.class, JSON2Object.class, LinkedHashMap.class, EnumMap.class, Hashtable.class, TreeMap.class};

    /**
     * Mapに使える実装を適当に決める
     *
     * @param type
     * @return
     */
    static Map typeToMap(Type type) {
        Class tcls;
        if (type instanceof ParameterizedType) {
            tcls = (Class) ((ParameterizedType) type).getRawType();
        } else {
            tcls = (Class) type;
        }

        for (Class cls : MAPS) {
            if (tcls.isAssignableFrom(cls)) {
                try {
                    return (Map) cls.getConstructor().newInstance();
                } catch (NoSuchMethodException | SecurityException | InstantiationException
                        | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
                    Logger.getLogger(OMAPConvert.class.getName()).log(Level.SEVERE, null, ex);
                    throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
                }
            }
        }

        try {
            return (Map) tcls.getConstructor().newInstance();
        } catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException
                | IllegalArgumentException | InvocationTargetException ex) {
            Logger.getLogger(OMAPConvert.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
    

    static Class<? extends Collection>[] COLL = new Class[]{JSON2Array.class, ArrayList.class, HashSet.class, LinkedList.class};

    static Collection typeToList(Class cls) {
        for (Class<? extends Collection> colCls : COLL) {
            if (cls.isAssignableFrom(colCls)) {
                try {
                    return colCls.getConstructor().newInstance();
                } catch (NoSuchMethodException | SecurityException | InstantiationException
                        | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
                    Logger.getLogger(OMAPConvert.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }

        try {
            return (Collection) cls.getConstructor().newInstance();
        } catch (NoSuchMethodException | SecurityException | InstantiationException | IllegalAccessException
                | IllegalArgumentException | InvocationTargetException ex) {
            Logger.getLogger(OMAPConvert.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
//        throw new UnsupportedOperationException("未サポートな型:" + cls.getTypeName());
    }
}
