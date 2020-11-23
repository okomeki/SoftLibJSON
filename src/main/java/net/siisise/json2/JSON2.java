package net.siisise.json2;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;
import java.util.stream.Collector;
import net.siisise.json.JSON;
import net.siisise.json.JSONFormat;
import net.siisise.omap.OMAP;

/**
 * 中間形式をListとMap対応にして実質なくした版
 * JSON文字列 toString()
 * JSON2Object valueOf()
 * List/Mapプリミティブ列 map()
 * JavaObjectマップ typeMap()
 * JSONP toJson()
 */
public interface JSON2 {
   
    static class ParameterizedTypeImpl implements ParameterizedType {
        private Type rawType;
        private Type[] args;
        
        ParameterizedTypeImpl(Type raw, Type[] args) {
            rawType = raw;
            this.args = args;
        }
        
        ParameterizedTypeImpl(Type... clss) {
            rawType = clss[0];
            args = new Type[clss.length - 1];
            System.arraycopy(clss, 1, args, 0, clss.length - 1);
        }

        @Override
        public Type[] getActualTypeArguments() {
            return args;
        }

        @Override
        public Type getRawType() {
            return rawType;
        }

        /**
         * 使わない
         */
        @Override
        public Type getOwnerType() {
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }
    }
    
    static Type parameterizedType(Type... clss) {
        return new ParameterizedTypeImpl(clss);
    }
    
    static Type listType(Type c) {
        return new ParameterizedTypeImpl(List.class, c);
    }

    /**
     * JSON文字列からObjectにパースする.
     * 
     * @param json
     * @return 
     */
    static Object parse(String json) {
        return JSON28259Reg.parse(json);
    }
    
    static JSON2Value parseWrap(String json) {
        return valueWrap(JSON28259Reg.parse(json));
    }

    /**
     * JSONデータ列からObjectにパースする
     * @param json
     * @return 
     */
    static Object parse(byte[] json) {
        return JSON28259Reg.parse(json);
    }

    static JSON2Value parseWrap(byte[] json) {
        return valueWrap(JSON28259Reg.parse(json));
    }

    public static JSONFormat NOBR = JSON.NOBR;
    public static JSONFormat TAB = JSON.TAB;
    
    /**
     * JSON (JavaのString)として出力する.
     * @return JSON
     */
    @Override
    String toString();
    /**
     * 書式を指定してJSONとして出力する.
     * @param format
     * @return 
     */
    String toString(JSONFormat format);
    
    /**
     * JSON中間型(風) Listまたは Map型で返す。 
     * @param src
     * @return JSON2系ListとMapのJavaっぽいデータ
     */
    public static Object valueMap(Object src) {
        return OMAP.valueOf(src, Object.class);
    }
    
    /**
     * valueOf で結果が単体(primitive型)のときラップしてから返す
     * @param src
     * @return 
     */
    public static JSON2Value valueOf(Object src) {
        return OMAP.valueOf(src, JSON2Value.class);
    }
    
    static JSON2Value valueWrap(Object val) {
        if ( val == null ) {
            return JSON2NULL.NULL;
        } else if ( val instanceof JSON2Value ) {
            return (JSON2Value)val;
        } else if ( val instanceof Boolean ) {
            return (Boolean)val ? JSON2Boolean.TRUE : JSON2Boolean.FALSE;
        } else if ( val instanceof Number ) {
            return new JSON2Number((Number)val);
        } else if ( val instanceof CharSequence ) {
            return new JSON2String((CharSequence)val);
        }
        throw new UnsupportedOperationException("未" + val.getClass().getName());
    }

    /**
     * 要素をそのまま維持してJSON2Arrayにする。
     * @param <T>
     * @return 
     */
    public static <T> Collector<T,?,JSON2Array> toJSON2Array() {
        return Collector.of(
                JSON2Array::new,
                JSON2Array::add,
                (ls, vals) -> {
                    ls.addAll(vals);
                    return ls;
                },
                Collector.Characteristics.IDENTITY_FINISH
        );
    }

    /**
     * StreamからREST寄りのprimitive型に変換して格納する。
     * @param <T>
     * @return 
     */
    public static <T> Collector<T,?,JSON2Array> toJSON2PrimArray() {
        return Collector.of(
                JSON2Array::new,
                JSON2Array::addValue,
                (ls, vals) -> {
                    vals.forEach(ls::addValue);
                    return ls;
                },
                Collector.Characteristics.IDENTITY_FINISH
        );
    }
}
