package net.siisise.json.bind.target;

import java.util.Collection;
import java.util.Map;
import java.util.stream.Collectors;
import net.siisise.bind.Rebind;
import net.siisise.bind.format.TypeFallFormat;
import net.siisise.bind.format.BindObject;
import net.siisise.bind.format.ContentBind;

/**
 * エスケープしないJSONっぽいもの.
 * @deprecated 使い処が見つからない
 */
@Deprecated
public class StringConvert extends TypeFallFormat<String> implements BindObject<String>, ContentBind<String> {

    @Override
    public String contentType() {
        return "text/plain";
    }
    
    @Override
    public String nullFormat() {
        return "null";
    }

    @Override
    public String booleanFormat(boolean bool) {
        return Boolean.toString(bool);
    }

    @Override
    public String numberFormat(Number num) {
        return num.toString();
    }

    @Override
    public String stringFormat(String str) {
        return str;
    }

    @Override
    public String arrayFormat(Object array) {
        Class cls = array.getClass();
        Class componentClass = cls.getComponentType();
        if ( componentClass == Character.TYPE ) {
            return String.valueOf((char[])array);
        }
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public String collectionFormat(Collection list) {
        return (String)list.stream().map(v -> { return Rebind.valueOf(v, this);})
                .collect( Collectors.joining(",", "[", "]"));
    }

    @Override
    public String mapFormat(Map map) {
        StringBuilder mapsb = new StringBuilder();
        ((Map<?,?>)map).forEach((k,v) -> { mapsb.append(Rebind.valueOf(k, this)).append(Rebind.valueOf(v, this));});
        return mapsb.toString();
    }

    @Override
    public String objectFormat(Object obj) {
        return obj.toString();
    }
}
