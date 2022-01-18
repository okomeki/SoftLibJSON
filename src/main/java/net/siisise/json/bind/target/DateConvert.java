package net.siisise.json.bind.target;

import java.lang.reflect.Type;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import net.siisise.json.bind.MtoConvert;

/**
 * NumberとStringに変換できるだけ
 */
public class DateConvert implements MtoConvert {

    @Override
    public Type targetClass() {
        return Date.class;
    }

    @Override
    public Object nullValue() {
        return null;
    }

    @Override
    public Object booleanValue(Boolean bool) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Object numberValue(Number num) {
        return new Date(num.longValue());
    }

    static String ISO = "yyyy-MM-dd'T'HH:mm:ss.SSSX";
    static String ISO2 = "yyyy-MM-dd'T'HH:mm:ssX";

    @Override
    public Object stringValue(CharSequence str) {
        try {
            SimpleDateFormat format;
            String val = str.toString();
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
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Object listValue(Collection list) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Object mapValue(Map map) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Object objectValue(Object obj) {
        if (obj instanceof Date) {
            return obj;
        }
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Object arrayValue(Object array) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
}
