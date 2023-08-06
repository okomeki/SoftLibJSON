/*
 * Copyright 2022 okome.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.siisise.json.bind.target;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.GenericArrayType;
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
import net.siisise.bind.format.TypeBind;
import net.siisise.io.BASE64;
import net.siisise.json.jsonp.JSONPArray;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONObject;
import net.siisise.json.JSONString;
import net.siisise.json.bind.OMAP;
import net.siisise.json.JSONValue;

/**
 * String Integer, 継承関係型 JSON2NULL など継承、未解決型への変換.
 * あとで分ける
 */
public class OMAPConvert extends OBJConvert<Object> implements TypeBind<Object> {
    
    Type type;
    
    public OMAPConvert(Type cls) {
        this.type = cls;
    }

    @Override
    public Type targetClass() {
        return type;
    }

    @Override
    public Object nullFormat() {
        return null;
    }

    @Override
    public Object booleanFormat(boolean bool) {
        if ( type instanceof Class ) {
            Class cls = (Class) type;
            if ( cls.isAssignableFrom(Boolean.class) || cls == Boolean.TYPE ) {
                return (Boolean)bool;
            } else if ( cls == String.class || cls == CharSequence.class ) {
                return Boolean.toString(bool);
            } else if ( cls.isAssignableFrom(JSONBoolean.class) ) {
                return (bool ? JSONBoolean.TRUE : JSONBoolean.FALSE);
//            } else if ( cls.isAssignableFrom(Integer.class) ) {
//                return Integer.valueOf( bool ? 1 : 0 );
            } else if ( cls.isAssignableFrom(Number.class)) {
                return numberFormat( Integer.valueOf( bool ? 1 : 0 ) );
//            } else if ( cls.isAssignableFrom(Byte.class) ) {
//                return Byte.valueOf( bool ? (byte)1 : (byte)0 );
//            } else if ( cls.isAssignableFrom(Short.class) ) {
//                return Short.valueOf( bool ? (short)1 : (short)0 );
//            } else if ( cls.isAssignableFrom(Long.class) ) {
//                return Long.valueOf( bool ? 1l : 0l );
            }
        }
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Object numberFormat(Number number) {
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
            if ( number instanceof BigInteger ) {
                return new BigDecimal((BigInteger)number);
            }
            return new BigDecimal(number.toString());
        } else if ( cls == String.class || cls == CharSequence.class ) {
            return number.toString();
        }

        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    /**
     * CharSequence に変えても使えるように残す
     * @param value
     * @return 
     */
    @Override
    public Object stringFormat(String value) {
        if (value == null) {
            return nullFormat();
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
            if ( cls.isArray() ) {
                if ( cls.getComponentType() == Character.TYPE ) {
                    return val.toCharArray();
                } else if ( cls.getComponentType() == Byte.TYPE ) {
                    BASE64 b64 = new BASE64(BASE64.URL,0);
                    return b64.decode(val);
                }
                throw new UnsupportedOperationException("謎の配列");
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
    public Object collectionFormat(Collection list) {
        if (type instanceof Class) {
            return listClassCast(list, (Class)type);
        } else if ( type instanceof ParameterizedType ) { // List<Generic>
            ParameterizedType pt = (ParameterizedType) type;
            Type raw = pt.getRawType();
            Collection col = typeToList((Class)raw);
            if ( col != null ) {
                return listCollectionTypeMap(list, pt, col);
            }

        } else if ( type instanceof GenericArrayType ) { // XXX<String>[]
            GenericArrayType gat = (GenericArrayType)type;
            Type component = gat.getGenericComponentType();
            return listToArray(list, component);
        }
        
        throw new UnsupportedOperationException("未サポートな型:" + type.getTypeName());
    }

    /**
     * 
     * @param <I>
     * @param <T>
     * @param src
     * @param cls
     * @return 
     */
    public static <I,T> T listClassCast(Collection<I> src, Class<T> cls) {
        if (cls == String.class || cls == CharSequence.class) {
            return (T) src.toString();
        } else if (cls.isAssignableFrom(src.getClass())) {
            return (T) src; // ToDo: 複製?
        } else if (!cls.isAssignableFrom(List.class) && cls.isAssignableFrom(JsonArray.class)) { // List を除く
            if ( src.isEmpty() ) {
                return (T)JsonValue.EMPTY_JSON_ARRAY;
            }
            return (T) src.stream().collect(JSONPArray.collector());
        } else if (cls.isArray()) { // 配列 要素の型も指定可能, Memberの型ではParameterizedTypeに振り分けられそう?
            return (T) listToArray(src, cls.getComponentType());
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
     * Genericを外す仮
     * @param type
     * @return 
     */
    private static Class unGene( Type type ) {
        if ( type instanceof ParameterizedType ) {
            return (Class) ((ParameterizedType) type).getRawType();
        } else if ( type instanceof GenericArrayType ) {
            Type gct = ((GenericArrayType) type).getGenericComponentType();
            Class c = unGene(gct);
            return Array.newInstance(c, 0).getClass(); // 配列の型を作る方法は? arrayType() JDK12以降
        } else if ( type instanceof Class ) {
            return (Class) type;
        }
        throw new UnsupportedOperationException();
    }
    
    /**
     * Genericを外した形で作ればいいのかどうか
     * @param <I>
     * @param src
     * @param componentType
     * @return 
     */
    private static <I> Object listToArray(Collection<I> src, Type componentType) {
        Class raw = unGene(componentType);
        Object array = Array.newInstance(raw, src.size());
        
        int i = 0;
        for ( I val : src ) {
            if ( val instanceof JSONValue ) { // 中身はGeneric対応で変換
                Array.set(array, i++, ((JSONValue) val).typeMap(componentType));
            } else {
                Array.set(array, i++, OMAP.valueOf(val,componentType));
            }
        }
        return array;
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
            list.stream().map(m -> OMAP.valueOf(m,argTypes[0])).forEach(col::add);
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
                    if (o instanceof JSONValue) {
                        params[i] = ((JSONValue) o).typeMap(pt[i]);
                    } else {
                        params[i] = OMAP.valueOf(o, pt[i]);
                    }
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
    public Object mapFormat(Map map) {
        if (type instanceof Class) {
            return mapClassCast(map, (Class)type);
        } else if ( type instanceof ParameterizedType ) {
            return mapParameterizedCast(map, (ParameterizedType)type);
        }
        throw new UnsupportedOperationException("まだない");
    }

    /**
     * Mapを指定の型に収まるよう加工する
     * @param type
     * @return
     */
    private <K,V> Map mapParameterizedCast(Map<K,V> src, ParameterizedType type) {
        Type raw = type.getRawType();
        if ((raw instanceof Class) && (Map.class.isAssignableFrom(((Class) raw)))) {
            Type[] args = type.getActualTypeArguments();
            Map map = typeToMap((Class) raw);
            src.forEach((k,v) -> {
                Object tkey = OMAP.valueOf(k, args[0]);
                Object tval = OMAP.valueOf(v, args[1]);
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
            obj.forEach((k,v) -> {
                map.put(k, v);
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

    static Class[] MAPS = {HashMap.class, JSONObject.class, LinkedHashMap.class, EnumMap.class, Hashtable.class, TreeMap.class};

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
    

    static Class<? extends Collection>[] COLL = new Class[]{JSONArray.class, ArrayList.class, HashSet.class, LinkedList.class};

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
