package net.siisise.omap.target;

import java.util.Collection;
import java.util.Map;
import net.siisise.json2.JSON2Array;
import net.siisise.json2.JSON2Boolean;
import net.siisise.json2.JSON2NULL;
import net.siisise.json2.JSON2Number;
import net.siisise.json2.JSON2Object;
import net.siisise.json2.JSON2String;
import net.siisise.json2.JSON2Value;
import net.siisise.omap.OMAP;

/**
 * List,Mapは表面をJSON2系にラップするだけと、中もprimitive系に更新するのと2種類想定するかもしれない
 */
public class JSON2Convert extends OBJConvert<JSON2Value> {

    @Override
    public Class<JSON2Value> targetClass() {
        return JSON2Value.class;
    }

    @Override
    public JSON2NULL nullValue() {
        return JSON2NULL.NULL;
    }

    @Override
    public JSON2Boolean booleanValue(Boolean bool) {
        return bool ? JSON2Boolean.TRUE : JSON2Boolean.FALSE;
    }

    @Override
    public JSON2Number numberValue(Number num) {
        return new JSON2Number(num);
    }

    @Override
    public JSON2String stringValue(CharSequence str) {
        return new JSON2String(str);
    }

    /**
     * 表面のみ変換
     * @param list
     * @return 
     */
    @Override
    public JSON2Array listValue(Collection list) {
        if ( list instanceof JSON2Array ) {
            return (JSON2Array) list;
        }
        return new JSON2Array(list);
    }

    /**
     * 表面のみ変換
     * @param map
     * @return 
     */
    @Override
    public JSON2Object mapValue(Map map) {
        if ( map instanceof JSON2Object ) {
            return (JSON2Object) map;
        }
        JSON2Object obj = new JSON2Object();
        ((Map<Object,Object>)map).forEach((k,v) -> {
            obj.put(k.toString(), v);
        });
        return obj;
    }

    /**
     * 表面のみ変換
     * @param obj
     * @return 
     */
    @Override
    public JSON2Value objectValue(Object obj) {
        // toJSON メソッドで変換
        JSON2Value json = OMAP.toJSON(obj);
        if ( json != null ) {
            return json;
        }
        return super.objectValue(obj);
    }
}
