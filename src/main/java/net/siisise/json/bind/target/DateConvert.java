package net.siisise.json.bind.target;

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
public class DateConvert extends TypeFallFormat<Date> implements TypeBind<Date>, BindObject<Date> {

    @Override
    public Date nullFormat() {
        return null;
    }

    @Override
    public Date booleanFormat(boolean bool) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Date numberFormat(Number num) {
        return new Date(num.longValue());
    }

    static String ISO = "yyyy-MM-dd'T'HH:mm:ss.SSSX";
    static String ISO2 = "yyyy-MM-dd'T'HH:mm:ssX";

    @Override
    public Date stringFormat(String val) {
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
    public Date collectionFormat(Collection list) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Date mapFormat(Map map) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Date objectFormat(Object obj) {
        if (obj instanceof Date) {
            return (Date) obj;
        }
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public Date arrayFormat(Object array) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}
