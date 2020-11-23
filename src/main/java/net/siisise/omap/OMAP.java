package net.siisise.omap;

import net.siisise.omap.target.JavaConvert;
import net.siisise.omap.target.JSON2Convert;
import net.siisise.omap.target.JSON1Convert;
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
import net.siisise.json.JSON;
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
 * SoftLibJSON から Object Mapping を分離する。 PoJoとか言われているらしいもの?
 * REST準拠なデータを持つList(JSONArrayだったもの), Map(JSONObjectだったもの) から各種変換をする便利機能。
 *
 * ToDo: JSON2Stringなどで Stringがデータのみの場合と書式込みの場合をどうにかして分ける
 *
 */
public class OMAP {

    /**
     * 中間 Java/JSON型振り分け
     */
    static OMConvert[] OMDS = {
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
    static MtoConvert[] OUTTYPES = {
        new JsonpConvert(),
        new JsonValueTypeConvert(),
        new JSON1Convert(),
        new JSON2Convert(),
        new DateConvert(), // 別にしたい
        new StringConvert(),
        new JavaConvert() // なにもしない
    };

    public static final Map<Class, List<OMConvert>> OMMAP = new HashMap<>();

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
     * @param type
     * @return
     */
    static MtoConvert convert(Type type) {
        MtoConvert pjc = CONVS.get(type);
        if (pjc == null) {
            pjc = new OMAPConvert(type);
        }
        return pjc;
    }

    public static <T> T valueOf(Object src, Type target) {
        MtoConvert<T> pjc = convert(target);
        return valueOf(src, pjc);
    }

    public static <T> T valueOf(Object src, MtoConvert<T> pjc) {
        List<OMConvert> ms;
        synchronized (OMMAP) {
            ms = OMMAP.get(src == null ? null : src.getClass());
        }
        if (ms != null) {
//            System.out.println("ヒット" + (src == null ? null : src.getClass()));
//            if ( ms.size() > 1 ) { // 型が同じとき変換しない以外にないかもしれないので省略するかも
//                System.out.println("多重" + ms.size());
//            }
            for (OMConvert om : ms) {
                T val = om.valueOf(src, pjc);
                if (val != om) {
                    return val;
                }
            }
        }
        // System.out.println("キャッシュミス" + src.getClass());
        // ヒットしないので一から探すすもしれない
        for (OMConvert ps : OMDS) {
            T val = ps.valueOf(src, pjc);
            if (val != ps) {
                putOMS(src.getClass(), ps);
                return val;
            }
        }
        // JSON2ObjectM で必ず何か返す
        throw new UnsupportedOperationException();
    }

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
     * あとで分ける.
     *
     * @param <T> てきとう
     * @param value primitive,Collection寄りな値
     * @param type
     * @return
     */
    public static <T> T typeValue(Object value, Type type) {
        if (type instanceof Class) {
            if (type == String.class) {
                return (T) valueOf(value, JSON2Value.class).toString();
            } else if (JSON.class.isAssignableFrom((Class) type)) {
                JSON v = JSON.valueOf(value);
                if (((Class) type).isAssignableFrom(v.getClass())) {
                    return (T) v;
                } else {
                    throw new ClassCastException("JSONの型が不一致");
                }
            }
        }
        throw new UnsupportedOperationException("まだない");
    }

    public static <T> T typeNull(Type type) {
        MtoConvert<T> cnv = convert(type);
        return cnv.nullValue();
    }

    public static <T> T typeBoolean(boolean bool, Type type) {
        MtoConvert<T> cnv = convert(type);
        return cnv.booleanValue(bool);
    }

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

    public static String toJSON(Object obj) {
        Class<? extends Object> cls = obj.getClass();
        try {
            Method toj = cls.getMethod("toJSON");
            return (String) toj.invoke(obj);
        } catch (NoSuchMethodException ex) {
            // 特にないので標準の変換へ
        } catch (SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            Logger.getLogger(JSON2Convert.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
}
