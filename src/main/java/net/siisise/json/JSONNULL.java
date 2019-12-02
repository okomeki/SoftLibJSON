package net.siisise.json;

/**
 * null
 */
public class JSONNULL extends JSONValue {

    public JSONNULL() {

    }

    @Override
    public Object value() {
        return null;
    }

    @Override
    public String toString() {
        return "null";
    }

    @Override
    public String toString(JSONFormat format) {
        return "null";
    }

    @Override
    public Object map() {
        return null;
    }

    @Override
    public Object map(Class cls) {
        return null;
    }

}
