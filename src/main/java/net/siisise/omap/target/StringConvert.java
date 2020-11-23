package net.siisise.omap.target;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.Map;
import net.siisise.omap.MtoConvert;

/**
 *
 */
public class StringConvert implements MtoConvert<String> {

    @Override
    public Type targetClass() {
        return String.class;
    }

    @Override
    public String nullValue() {
        return null;
    }

    @Override
    public String booleanValue(Boolean bool) {
        return Boolean.toString(bool);
    }

    @Override
    public String numberValue(Number num) {
        return num.toString();
    }

    @Override
    public String stringValue(CharSequence str) {
        return str.toString();
    }

    @Override
    public Object listValue(Collection list) {
        return list.toString();
    }

    @Override
    public Object mapValue(Map map) {
        return map.toString();
    }

    @Override
    public Object objectValue(Object obj) {
        return obj.toString();
    }
    
}
