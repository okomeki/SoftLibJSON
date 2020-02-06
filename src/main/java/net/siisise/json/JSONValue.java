package net.siisise.json;

import java.util.Map;

/**
 * 基本型
 * @param <T>
 */
public abstract class JSONValue<T> implements JSON<T> {

    T value;

    @Override
    public T value() {
        return value;
    }
    
    @Override
    public String toString() {
        return toString(TAB);
    }

    /**
     * 改行を少しなんとかする。
     * @param format TABかNOBR
     * @return 
     */
    @Override
    public String toString(JSONFormat format) {
        return value.toString();
    }

    String tab(String val) {
        return val.replace("\r\n", "\r\n  ");
    }
    
    @Override
    public <E> E map(Map<Class,JSONReplaceMO> mp, Class<E> src ) {
        return map(src);
    }
    
    /**
     * ParserにstaticでvalueOfを実装してみる
     * Replacer としてあとでまとめる
     */
    static JSONMap PARSERS = new JSONMap();

    /**
     * 
     * @param src
     * @return 
     */
    public static JSONValue valueOf(Object src) {
        return valueOf(src, null);
    }

    /**
     * なんでもJSONに変換する。
     * プリミティブ型、配列、Collection、Object boolean byte short char int long float
     * double List Map Number null String
     * Date型など要検討
     * @param src データ型なんでも
     * @param replacer
     * @return JSONValue
     */
    public static JSONValue valueOf(Object src, JSONReplacer replacer) {
        return PARSERS.valueOf(src,replacer);
    }

    /**
     *
     * @param o
     * @return
     */
    @Override
    public boolean equals(Object o) {
        return o != null && getClass() == o.getClass()
                && ((value == null && ((JSONValue) o).value == null) || value.equals(((JSONValue) o).value));
    }
}
