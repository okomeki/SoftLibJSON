package net.siisise.json;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
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

    /**
     * 
     * @param <E>
     * @param mp
     * @param t
     * @return 
     */
    @Override
    public <E> E typeMap(Map<Class,JSONReplaceMO> mp, Type t ) {
        if ( t instanceof ParameterizedType ) {
            ParameterizedType pt = (ParameterizedType) t;
            Type[] atas = pt.getActualTypeArguments();
/*
            for ( Type ata : atas ) {
                System.out.println( " ata.class : "  + ata.getClass().getName() );
                System.out.println( " ata.typeName : " + ata.getTypeName() );
                if ( ata instanceof Class ) {
                    System.out.println( " ata.name : " + ((Class) ata).getName() );
                }
            }
*/
            Type raw = pt.getRawType();
            System.out.println("pt.rawtype:" + raw.getTypeName());
//            System.out.println("pt.ownertype:" + pt.getOwnerType().getTypeName());
            JSONReplaceMO conv = mp.get(raw);
            if ( conv != null ) {
                return (E)conv.replace(this, (Class) raw);
            }
            return typeMap(t);
        }
        JSONReplaceMO conv = mp.get(t);
        if ( conv != null ) {
            return (E)conv.replace(this, (Class) t);
        }
        return typeMap(t);
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
