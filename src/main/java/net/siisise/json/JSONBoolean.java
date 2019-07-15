package net.siisise.json;

/**
 *
 * @author okome
 */
public class JSONBoolean extends JSONValue<Boolean> {

    public JSONBoolean(Boolean b) {
        value = b;
    }

    @Override
    public Boolean map() {
        return value;
    }

    /**
     * boolean,Boolean,String ぐらいに対応
     *
     * @param <T>
     * @param cls
     * @return
     */
    @Override
    public <T> T map(Class<T> cls) {
//        if ( Boolean.TYPE == cls ) {
//            return (T)value;
//        }
        return (T) value;
    }

}
