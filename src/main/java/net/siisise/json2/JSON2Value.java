package net.siisise.json2;

import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.Map;
import javax.json.JsonValue;
import net.siisise.json.JSONReplaceMO;

/**
 * 
 * JSON2Object implements Map
 * JSON2Array implements List
 * JSON2Number implements Number
 * JSON2String 互換なし
 * JSON2Boolean 互換なし
 * JSON2NULL 互換なし
 *
 * toString() をJSON出力にすること
 */
public interface JSON2Value extends JSON2 {
    
    /**
     * JSON TEXT
     * @return 
     */
    @Override
    String toString();
    
    /**
     * 固定のJava寄りの型に変換する。
     * List,Map,primitive対応データ型など
     * @param <T>
     * @return 
     */
    <T> T map();
    
    /**
     * 可能な限り指定型に変換する。
     * List,Map,配列,Java Object, Genericなどまで可
     * @param <T>
     * @param type
     * @return 
     */
    <T> T typeMap(Type type);
    default <T> T typeMap(Type type, Map<Class,JSONReplaceMO> mp) {
        if ( type instanceof ParameterizedType ) {
            ParameterizedType pt = (ParameterizedType) type;
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
//            System.out.println("pt.rawtype:" + raw.getTypeName());
//            System.out.println("pt.ownertype:" + pt.getOwnerType().getTypeName());
            JSONReplaceMO conv = mp.get(raw);
            if ( conv != null ) {
                return (T)conv.replace(this, (Class) raw);
            }
            typeMap(type);
        }
        JSONReplaceMO conv = mp.get(type);
        if ( conv != null ) {
            return (T)conv.replace(this, (Class) type);
        }
        return typeMap(type);
    }
    
    /**
     * JSONP系に準拠する.
     * @return 
     */
    JsonValue toJson();

}
