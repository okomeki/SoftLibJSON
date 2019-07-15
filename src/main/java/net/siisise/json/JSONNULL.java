package net.siisise.json;

/**
 *
 * @author okome
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
    public Object map() {
        return null;
    }

    @Override
    public Object map(Class cls) {
        return null;
    }
    
}
