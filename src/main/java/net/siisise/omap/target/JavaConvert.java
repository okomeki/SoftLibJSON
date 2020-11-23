package net.siisise.omap.target;

import java.util.Collection;
import java.util.Map;
import net.siisise.omap.MtoConvert;

/**
 * Java Object を Java Object にするので何もしないかもしれない。
 * 特定型に変換するにはOMAPConvert などを使用する
 */
public class JavaConvert implements MtoConvert {

    @Override
    public Class targetClass() {
        return Object.class;
    }

    @Override
    public Object nullValue() {
        return null;
    }

    @Override
    public Object booleanValue(Boolean bool) {
        return bool;
    }

    @Override
    public Object numberValue(Number num) {
        return num;
    }

    @Override
    public Object stringValue(CharSequence str) {
        return str.toString();
    }

    @Override
    public Object listValue(Collection list) {
        return list;
    }

    @Override
    public Object mapValue(Map map) {
        return map;
    }

    @Override
    public Object objectValue(Object obj) {
        return obj;
    }
    
}
