package net.siisise.omap;

import net.siisise.omap.target.JavaConvert;
import net.siisise.omap.target.JSON2Convert;
import net.siisise.omap.target.JsonpConvert;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonValue;
import net.siisise.json.map.JSONDateM;
import net.siisise.json.map.JSONUUIDM;
import net.siisise.json2.JSON2Value;
import net.siisise.omap.source.JSON2ArrayM;
import net.siisise.omap.source.JSON2NumberM;
import net.siisise.omap.source.JSON2ObjectM;
import net.siisise.omap.source.JSON2StringM;
import net.siisise.omap.source.JSON2ValueM;
import net.siisise.omap.target.DateConvert;
import net.siisise.omap.target.JsonValueTypeConvert;
import net.siisise.omap.target.OMAPConvert;
import net.siisise.omap.target.StringConvert;

/**
 * SoftLibJSON から Object Mapping を分離する。
 * PoJoとか言われているらしいもの?
 * REST準拠なデータを持つList(JSONArrayだったもの), Map(JSONObjectだったもの) から各種変換をする便利機能。
 *
 * ToDo: JSON2Stringなどで Stringがデータのみの場合と書式込みの場合をどうにかして分ける
 *
 */
public class OMAP {

    /**
     * 中間 Java/JSON型振り分け
     */
    static final OMConvert[] OMDS = {
        new JSON2ValueM(),
        new JSON2NumberM(),
        new JSONUUIDM(),
        new JSONDateM(),
        new JSON2StringM(),
        new JSON2ArrayM(),
        new JSON2ObjectM()
    };

    /**
     * 出力別
     */
    static final MtoConvert[] OUTTYPES = {
        new JsonpConvert(),
        new JsonValueTypeConvert(),
        new JSON2Convert(),
        new DateConvert(), // 別にしたい
        new StringConvert(),
        new JavaConvert() // なにもしない
    };

    /**
     * 型別に整理したOMDS
     */
    public static final Map<Class, List<OMConvert>> OMMAP = new HashMap<>();

    /**
     * 型別に整理したOUTTYPES
     */
    public static Map<Type, MtoConvert> CONVS = new HashMap();

    static {
        for (OMConvert om : OMDS) {
            Class[] srcs = om.getSrcClasses();
            for (Class src : srcs) {
                putOMS(src, om);
            }
        }
        List<OMConvert> nulls = new ArrayList();
        nulls.add(new JSON2ValueM());
        OMMAP.put(null, nulls);

        for (MtoConvert cn : OUTTYPES) {
            CONVS.put(cn.targetClass(), cn);
        }
    }

    /**
     * ToDo: 拡張可能にする
     *
     * @param type 親クラスだとはやい
     * @return 出力先変換器
     */
    static MtoConvert convert(Type type) {
        MtoConvert pjc = CONVS.get(type);
        if (pjc == null) {
            for ( Map.Entry<Type, MtoConvert> e : CONVS.entrySet() ) {
                Type key = e.getKey();
                if ( key instanceof Class && type instanceof Class && key != Object.class ) { // 何もしないJavaConvertだけ除外
                    Class<?> cnvClass = (Class<?>)key;
                    Class<?> typeClass = (Class<?>)type;
                    if ( cnvClass.isAssignableFrom(typeClass) ) {
                        return e.getValue();
                    }
                }
            }
            pjc = new OMAPConvert(type);
        }
        return pjc;
    }

    public static <T> T valueOf(Object src, Type target) {
        MtoConvert<T> pjc = convert(target);
        return valueOf(src, pjc);
    }

    /**
     * 出力先が決まっているときの対応
     * @param <T>
     * @param src
     * @param pjc 出力型変換器
     * @return 
     */
    public static <T> T valueOf(Object src, MtoConvert<T> pjc) {
        List<OMConvert> omcs;
        synchronized (OMMAP) {
            omcs = OMMAP.get(src == null ? null : src.getClass());
        }
        if (omcs != null) {
            for (OMConvert om : omcs) { // ソースから変換可能なOMCを探す
                T val = om.valueOf(src, pjc);
                if (val != om) { // omが返れば未対応 srcから変換可能なので抜ける
                    return val;
                }
            }
        }
        // ヒットしないので一から探すすもしれない
        for (OMConvert ps : OMDS) {
            T val = ps.valueOf(src, pjc);
            if (val != ps) {
                putOMS(src == null ? null : src.getClass(), ps);
                return val;
            }
        }
        // JSON2ObjectM で必ず何か返す
        throw new UnsupportedOperationException();
    }

    /**
     * 入力変換器キャッシュの追加
     * @param cls nullあり
     * @param om 
     */
    private static void putOMS(Class cls, OMConvert om) {
        synchronized (OMMAP) {
            List<OMConvert> oms = OMMAP.get(cls);
            if (oms == null) {
                oms = new ArrayList<>();
                OMMAP.put(cls, oms);
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
        MtoConvert<T> cnv = convert(type);
        return cnv.nullValue();
    }

    /**
     * booleanから特定の型に変える
     * @param <T>
     * @param bool
     * @param type 出力型
     * @return booleanっぽいもの
     */
    public static <T> T typeBoolean(boolean bool, Type type) {
        MtoConvert<T> cnv = convert(type);
        return cnv.booleanValue(bool);
    }

    /**
     * 特定の型の数値っぼいものに変換する
     * @param <T>
     * @param number 該当する数値っぽいもの
     * @param type
     * @return type型のnumberっぽいもの
     */
    public static <T> T typeNumber(Number number, Type type) {
        MtoConvert<T> cnv = convert(type);
        return cnv.numberValue(number);
    }

    public static <T> T typeList(Collection value, Type type) {
        MtoConvert cnv = convert(type);
        return (T) cnv.listValue(value);
    }

    public static <T> T typeMap(Map value, Type type) {
        MtoConvert cnv = convert(type);
        return (T) cnv.mapValue(value);
    }

    public static <T> T typeObject(Object value, Type type) {
        MtoConvert cnv = convert(type);
        return (T) cnv.objectValue(value);
    }

    public static <T> T typeString(CharSequence value, Type type) {
        MtoConvert cnv = convert(type);
        return (T) cnv.stringValue(value);
    }

    /**
     * toJSONメソッドがあればJSONに変換し、なければその他の方法で変換する
     * @param obj null不可
     * @return 
     */
    public static String toJSON(Object obj) {
        Class<? extends Object> cls = obj.getClass();
        try {
            Method toj = cls.getMethod("toJSON");
            Class retType = toj.getReturnType();
            if ( JSON2Value.class.isAssignableFrom(retType) ) {
                return ((JSON2Value)toj.invoke(obj)).toString();
            } else if ( JsonValue.class.isAssignableFrom(retType) ) {
                return ((JsonValue)toj.invoke(obj)).toString();
            }
            return (String) toj.invoke(obj);
        } catch (NoSuchMethodException ex) {
            // 特にないので標準の変換へ
        } catch (SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            Logger.getLogger(JSON2Convert.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
}
