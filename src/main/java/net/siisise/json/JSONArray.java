package net.siisise.json;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * 配列またはList
 * 
 * @author okome
 */
public class JSONArray extends JSONCollection<List<JSONValue>> {

    public JSONArray() {
        value = new ArrayList<>();
    }

    public JSONArray(List<JSONValue> vals) {
        value = new ArrayList<>(vals);
    }

    @Override
    public List<JSONValue> value() {
        return new ArrayList(value);
    }

    /**
     * 要素に変換したもの
     *
     * @return
     */
    @Override
    public List map() {
        List list = new ArrayList();
        for (JSONValue v : value) {
            list.add(v.map());
        }
        return list;
    }
    
    static Class<? extends Collection>[] COLL = new Class[] {
        ArrayList.class, HashSet.class, LinkedList.class };

    @Override
    public <T> T map(Class<T> cls) {
        return map(new Class[] {cls});
    }
    /**
     * List または　配列?
     *
     * @param <T>
     * @param clss Collection または配列、JSONArray、それ以外は要素の型として扱う
     * @return
     */
    @Override
    public <T> T map(Class... clss) {
        Class cls = clss[0];
        if ( cls == String.class ) {
            return (T) toString();
        } else if ( cls.isAssignableFrom(this.getClass()) ) {
            return (T) this;
        }
        
        if (cls.isArray()) { // 配列 要素の型も指定可能
            Class componentType = cls.getComponentType();
            Object arr = Array.newInstance(componentType, value.size());
            
            int i = 0;
            for ( JSONValue obj : value ) {
                Array.set(arr, i++, obj.map(componentType));
            }
            return (T)arr;
            
//            return (T) list.toArray((Object[]) arr);
        }
        
        // Collection 要素の型は?
        for ( Class<? extends Collection> s : COLL ) {
            if ( cls.isAssignableFrom(s) ) {
                Collection col;
                
                try {
                    col = s.getConstructor().newInstance();
                    
                    if ( clss.length > 1 ) {
                        Class[] clb = new Class[clss.length - 1];
                        System.arraycopy(clss, 1, s, 0, clb.length);
                        
                        for ( JSONValue o : value ) {
                            if ( o instanceof JSONCollection ) {
                                col.add(((JSONCollection)o).map(clb));
                            } else {
                                col.add(o.map(clss[1]));
                            }
                        }
                    } else {
                        for ( JSONValue o : value ) {
                            col.add(o);
                        }
                    }
                    return (T)col;
                } catch (InstantiationException ex) {
                    Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
                } catch (IllegalAccessException ex) {
                    Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
                } catch (IllegalArgumentException ex) {
                    Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
                } catch (InvocationTargetException ex) {
                    Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
                } catch (NoSuchMethodException ex) {
                    Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
                } catch (SecurityException ex) {
                    Logger.getLogger(JSONArray.class.getName()).log(Level.SEVERE, null, ex);
                }
            }
        }
        throw new UnsupportedOperationException();
    }

    public void add(Object o) {
        value.add(valueOf(o));
    }

    public JSONValue get(int index) {
        return value.get(index);
    }
    
    @Override
    public JSONValue get(String key) {
        return value.get(Integer.parseInt(key));
    }
    
    @Override
    public void set(String key, Object o) {
        if ( key.equals("-")) {
            value.add(valueOf(o));
        } else {
            value.set(Integer.parseInt(key),valueOf(o));
        }
    }
    
    public void add(String key, Object o) {
        if ( key.equals("-")) {
            value.add(valueOf(o));
        } else {
            value.add(Integer.parseInt(key),valueOf(o));
        }
    }
    
    public void remove(String key) {
        value.remove(Integer.parseInt(key));
    }
    

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append("[\r\n");
        for (JSONValue val : value) {
            if (sb.length() > 3) {
                sb.append(",\r\n");
            }
            sb.append("  ");
            sb.append(tab(val.toString()));
        }
        sb.append("\r\n]");
        return sb.toString();
    }
    
    @Override
    public boolean equals(Object o) {
        if ( o instanceof JSONArray ) {
            return value.equals(((JSONArray)o).value);
        } else {
            return false;
        }
    }
}
