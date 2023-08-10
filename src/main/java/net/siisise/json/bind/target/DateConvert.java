package net.siisise.json.bind.target;

import java.lang.reflect.Type;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import net.siisise.bind.format.TypeFallFormat;
import net.siisise.bind.format.BindObject;
import net.siisise.bind.format.TypeBind;

/**
 * NumberとStringから変換できるだけ
 */
public class DateConvert extends TypeFallFormat implements TypeBind, BindObject {

    @Override
    public Type targetClass() {
        return Date.class;
    }

    @Override
    public Object nullFormat() {
        return null;
    }

    @Override
    public Object booleanFormat(boolean bool) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Object numberFormat(Number num) {
        return new Date(num.longValue());
    }

    static String ISO = "yyyy-MM-dd'T'HH:mm:ss.SSSX";
    static String ISO2 = "yyyy-MM-dd'T'HH:mm:ssX";

    @Override
    public Object stringFormat(String val) {
        try {
            SimpleDateFormat format;
            if (val.length() <= 20) {
                format = new SimpleDateFormat(ISO2);
            } else {
                format = new SimpleDateFormat(ISO);
            }

            Date d = format.parse(val);
            return d;
        } catch (ParseException ex) {
            Logger.getLogger(DateConvert.class.getName()).log(Level.SEVERE, null, ex);
        }
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Object collectionFormat(Collection list) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Object mapFormat(Map map) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Object objectFormat(Object obj) {
        if (obj instanceof Date) {
            return obj;
        }
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Object arrayFormat(Object array) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}
