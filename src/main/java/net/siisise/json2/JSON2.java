package net.siisise.json2;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.List;
import java.util.stream.Collector;
import javax.json.JsonValue;
import net.siisise.json.JSON;
import net.siisise.json.JSONFormat;
import net.siisise.json.JSONReplacer;
import net.siisise.json.jsonp.JSONPArray;

/**
 * 中間形式をListとMap対応にして実質なくした版
 * JSON文字列 toString()
 * JSON2Object valueOf()
 * List/Mapプリミティブ列 map()
 * JavaObjectマップ typeMap()
 * JSONP toJson()
 */
public interface JSON2 {
   
    static final JSON2Map PUBLIC = new JSON2Map();
    static JSON2Map PARSERS = PUBLIC;
    
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
    
    @Override
    String toString();
    String toString(JSONFormat format);
    
    /**
     * 
     * @param src
     * @return 
     */
    public static Object valueMap(Object src) {
        return valueMap(src, null);
    }
    
    /**
     * JSON中間型(風) Listまたは Map型で返す。 
     * @param src
     * @param replacer
     * @return JSON2系ListとMapのJavaっぽいデータ
     */
    public static Object valueMap(Object src, JSONReplacer replacer) {
        return PARSERS.valueOf(src, replacer);
    }
    
    /**
     * valueOf で結果が単体(primitive型)のときラップしてから返す
     * @param src
     * @return 
     */
    public static JSON2Value valueOf(Object src) {
        return valueWrap(valueMap(src, null));
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
        throw new UnsupportedOperationException("未");
    }

    static <T> Collector<T,?,JSON2Array> toJSON2Array() {
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
     * Streamから落とす用.
     * @param <T>
     * @return 
     */
    static <T> Collector<T,?,JSON2Array> toJSON2PrimArray() {
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
    
    static <T> Collector<JsonValue,?,JSONPArray> toJsonPArray() {
        return Collector.of(
                JSONPArray::new,
                JSONPArray::addValue,
                (ls, vals) -> {
                    vals.forEach(ls::addValue);
                    return ls;
                },
                Collector.Characteristics.IDENTITY_FINISH
        );
    }
}
