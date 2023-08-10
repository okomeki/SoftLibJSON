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
package net.siisise.json.bind;

import net.siisise.json.bind.target.JavaConvert;
import net.siisise.json.bind.target.JSONConvert;
import net.siisise.json.bind.target.JsonpConvert;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonValue;
import net.siisise.bind.TypeUnbind;
import net.siisise.bind.format.TypeFormat;
import net.siisise.bind.unbind.java.UnbindArray;
import net.siisise.bind.unbind.java.UnbindBoolean;
import net.siisise.bind.unbind.java.UnbindCollection;
import net.siisise.bind.unbind.java.UnbindMap;
import net.siisise.bind.unbind.java.UnbindNull;
import net.siisise.bind.unbind.java.UnbindNumber;
import net.siisise.bind.unbind.java.UnbindObject;
import net.siisise.bind.unbind.java.UnbindString;
import net.siisise.json.map.JSONDateM;
import net.siisise.bind.unbind.java.UnbindUUID;
import net.siisise.json.unbind.UnbindJSONNumber;
import net.siisise.json.unbind.UnbindJSONString;
import net.siisise.json.unbind.UnbindJSONValue;
import net.siisise.json.bind.target.DateConvert;
import net.siisise.json.bind.target.JsonValueTypeConvert;
import net.siisise.json.bind.target.OMAPConvert;
import net.siisise.json.bind.target.StringConvert;
import net.siisise.json.JSON;
import net.siisise.json.JSONValue;
import net.siisise.json.bind.target.MessagePackConvert;
import net.siisise.bind.format.BindObject;
import net.siisise.bind.format.TypeBind;

/**
 * JSON-B相当のObject Mapping。
 * PoJoとか言われているらしいもの?
 * REST準拠なデータを持つList(JSONArrayだったもの), Map(JSONObjectだったもの) から各種変換をする便利機能。
 *
 * ToDo: JSON2Stringなどで Stringがデータのみの場合と書式込みの場合をどうにかして分ける。
 * JSON 以外でも対応しているのでパッケージ位置は仮。
 * Rebind としてやや拡張したのでこちらは削除予定。
 * 
 * @deprecated Rebind に置き換えたら消す
 */
public class OMAP {

    /**
     * 中間 Java/JSON型振り分け
     */
    static final TypeUnbind[] UNBINDS = {
        new UnbindNull(),
        new UnbindBoolean(),
        new UnbindJSONValue(),
        new UnbindJSONNumber(),
        new UnbindNumber(),
        new UnbindUUID(),
        new JSONDateM(),
        new UnbindString(),
        new UnbindJSONString(),
        new UnbindArray(),
        new UnbindCollection(),
        new UnbindMap(),
        new UnbindObject()
    };

    /**
     * 出力別
     */
    static final TypeFormat[] OUTTYPES = {
//        new JsonxpConvert(),
        new JsonpConvert(),
        new JsonValueTypeConvert(),
        new JSONConvert(),
        new DateConvert(), // 別にしたい
        new StringConvert(),
        new MessagePackConvert(),
        new JavaConvert() // なにもしない
    };

    /**
     * 型別に整理したOMDS
     */
    public static final Map<Type, List<TypeUnbind>> UNBINDMAP = new HashMap<>();

    /**
     * 型別に整理したOUTTYPES
     */
    public static final Map<Type, TypeFormat> CONVS = new HashMap();

    static {
        for (TypeUnbind ub : UNBINDS) {
            Type[] srcs = ub.getSrcTypes();
            for (Type src : srcs) {
                putOMS(src, ub);
            }
        }
        List<TypeUnbind> nulls = new ArrayList();
        nulls.add(new UnbindJSONValue());
        UNBINDMAP.put(null, nulls);
        
        for (TypeFormat cn : OUTTYPES) {
            if ( cn instanceof TypeBind ) {
                CONVS.put(((TypeBind) cn).targetClass(), cn);
            }
        }
    }
    
    public static Type toClass(Type type) {
        if ( type instanceof ParameterizedType ) {
            return ((ParameterizedType) type).getRawType();
        }
        // いろいろあるけど略
        return type;
    }

    /**
     * ToDo: 拡張可能にする
     *
     * @param type 親クラスだとはやい
     * @return 出力先変換器
     */
    static TypeFormat convert(Type type) {
        TypeFormat pjc = CONVS.get(type);
        if (pjc == null ) {
            Type rawClass = toClass(type);
            
            if ( rawClass instanceof Class ) {
//                CONVS.entrySet().stream().filter(e -> (e.getKey() instanceof Class));
                for ( Map.Entry<Type, TypeFormat> e : CONVS.entrySet() ) {
                    Type key = e.getKey();
                    if ( key instanceof Class && key != Object.class ) { // 何もしないJavaConvertだけ除外
                        Class<?> cnvClass = (Class<?>)key;
                        Class<?> typeClass = (Class<?>)rawClass;
                        if ( cnvClass.isAssignableFrom(typeClass) ) {
                            return e.getValue();
                        }
                    }
                }
            }
            pjc = new OMAPConvert(type);
        }
        return pjc;
    }

    public static <T> T valueOf(Object src, Type target) {
        TypeFormat<T> format = convert(target);
        return valueOf(src, format);
    }
    
    public static <T> T valueOf(Object src, TypeFormat<T> format) {
        return valueOf(src, format, UNBINDS);
    }

    /**
     * 出力先が決まっているときの対応
     * @param <T>
     * @param src
     * @param pjc 出力型変換器
     * @param omds
     * @return 
     */
    static <T> T valueOf(Object src, TypeFormat<T> pjc, TypeUnbind[] omds) {
        List<TypeUnbind> omcs;
        synchronized (UNBINDMAP) {
            omcs = UNBINDMAP.get(src == null ? null : src.getClass());
        }
        if (omcs != null) {
            for (TypeUnbind om : omcs) { // ソースから変換可能なOMCを探す
                T val = om.valueOf(src, pjc);
                if (val != om) { // omが返れば未対応 srcから変換可能なので抜ける
                    return val;
                }
            }
        }
        // ヒットしないので一から探すすもしれない
        for (TypeUnbind ps : omds) {
            T val = ps.valueOf(src, pjc);
            if (val != ps) {
                putOMS(src == null ? null : src.getClass(), ps);
                return val;
            }
        }
        // UnbindJSONObject で必ず何か返す
        throw new UnsupportedOperationException();
    }

    /**
     * 入力変換器キャッシュの追加
     * @param cls nullあり
     * @param om 
     */
    private static void putOMS(Type cls, TypeUnbind om) {
        synchronized (UNBINDMAP) {
            List<TypeUnbind> oms = UNBINDMAP.get(cls);
            if (oms == null) {
                oms = new ArrayList<>();
                UNBINDMAP.put(cls, oms);
            }
            if (!oms.contains(om)) {
                oms.add(om);
            }
        }
    }

    /**
     * 特定の型のnullっぽいものを返す
     * @param <T>
     * @param type 出力型
     * @return nullに該当する概念
     */
    public static <T> T typeNull(Type type) {
        TypeFormat<T> cnv = convert(type);
        return cnv.nullFormat();
    }

    /**
     * booleanから特定の型に変える
     * @param <T>
     * @param bool
     * @param type 出力型
     * @return booleanっぽいもの
     */
    public static <T> T typeBoolean(boolean bool, Type type) {
        TypeFormat<T> cnv = convert(type);
        return cnv.booleanFormat(bool);
    }

    /**
     * 特定の型の数値っぼいものに変換する
     * @param <T>
     * @param number 該当する数値っぽいもの
     * @param type
     * @return type型のnumberっぽいもの
     */
    public static <T> T typeNumber(Number number, Type type) {
        TypeFormat<T> format = convert(type);
        return format.numberFormat(number);
    }

    public static <T> T typeString(String value, Type type) {
        TypeFormat cnv = convert(type);
        return (T) cnv.stringFormat(value);
    }

    public static <T> T typeList(Collection value, Type type) {
        TypeFormat format = convert(type);
        return (T) format.collectionFormat(value);
    }

    public static <T> T typeMap(Map value, Type type) {
        TypeFormat cnv = convert(type);
        return (T) cnv.mapFormat(value);
    }

    public static <T> T typeObject(Object value, Type type) {
        TypeFormat format = convert(type);
        JSONValue json = toJSON(value);
        if ( json != null ) {
            return (T)OMAP.valueOf(json, format);
        }
        if ( format instanceof BindObject ) {
            return (T) ((BindObject) format).objectFormat(value);
        }
        return (T) new UnbindObject().valueOf(value, format);
    }

    /**
     * toJSONメソッドがあればJSONに変換し、なければその他の方法で変換する
     * String のほか JSON2Value, JsonValue でも可
     * @deprecated 使わなくてよくなったはず
     * @param obj null不可
     * @return 
     */
    public static JSONValue toJSON(Object obj) {
        Class<? extends Object> cls = obj.getClass();
        try {
            Method toj = cls.getMethod("toJSON");
            Class retType = toj.getReturnType();
            if ( JSONValue.class.isAssignableFrom(retType) ) {
                return ((JSONValue)toj.invoke(obj));
            } else if ( JsonValue.class.isAssignableFrom(retType) ) {
                return JSON.valueOf((JsonValue)toj.invoke(obj));
            }
            return JSON.parseWrap((String) toj.invoke(obj));
        } catch (NoSuchMethodException ex) {
            // 特にないので標準の変換へ
        } catch (SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            Logger.getLogger(OMAP.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
}
