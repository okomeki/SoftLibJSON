package net.siisise.json;

import java.util.Map;

/**
 * 基本型
 * 実装のみImpl相当で使わないようにするかもしれない
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
     *
     * @param o
     * @return
     */
    @Override
    public boolean equals(Object o) {
        return o != null && getClass() == o.getClass()
                && ((value == null && ((JSONValue) o).value() == null) || value.equals(((JSONValue) o).value()));
    }

}
