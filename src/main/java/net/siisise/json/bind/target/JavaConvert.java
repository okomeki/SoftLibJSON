package net.siisise.json.bind.target;

import java.util.Collection;
import java.util.Map;
import net.siisise.bind.format.TypeFallFormat;
import net.siisise.bind.format.BindObject;
import net.siisise.bind.format.TypeBind;

/**
 * Java Object を Java Object にするので何もしないかもしれない。
 * 特定型に変換するにはOMAPConvert などを使用する
 */
public class JavaConvert extends TypeFallFormat<Object> implements TypeBind<Object>,BindObject<Object> {

    @Override
    public Object nullFormat() {
        return null;
    }

    @Override
    public Object booleanFormat(boolean bool) {
        return bool;
    }

    @Override
    public Object numberFormat(Number num) {
        return num;
    }

    @Override
    public Object stringFormat(String str) {
        return str;
    }

    @Override
    public Object arrayFormat(Object array) {
        return array;
    }

    @Override
    public Object collectionFormat(Collection list) {
        return list;
    }

    @Override
    public Object mapFormat(Map map) {
        return map;
    }

    @Override
    public Object objectFormat(Object obj) {
        return obj;
    }
    
}
