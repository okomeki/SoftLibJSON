package net.siisise.json.map;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import net.siisise.json.JSON;
import net.siisise.json.JSONReplaceMO;
import net.siisise.json.JSONReplaceOM;
import net.siisise.json.JSONReplacer;
import net.siisise.json.JSONValue;
import net.siisise.json2.JSON2ReplaceOM;
import net.siisise.json2.JSON2Value;

public class JSONDateM implements JSONReplaceMO<Date>,JSONReplaceOM,JSON2ReplaceOM {

    static String ISO = "yyyy-MM-dd'T'HH:mm:ss.SSSX";
    static String ISO2 = "yyyy-MM-dd'T'HH:mm:ssX";

    /**
     *
     * @return
     */
    @Override
    public Class<Date> targetClass() {
        return Date.class;
    }

    @Override
    public JSONValue valueOf(Object obj, JSONReplacer r) {
        if (obj instanceof Date) {
            SimpleDateFormat format = new SimpleDateFormat(ISO2);
            String s = format.format((Date) obj);
            return JSON.valueOf(s);
        }
        return null;
    }

    @Override
    public Object value2Of(Object obj, JSONReplacer replacer) {
        if (obj instanceof Date) {
            SimpleDateFormat format = new SimpleDateFormat(ISO2);
            String s = format.format((Date) obj);
            return s;
        }
        return null;
    }

    @Override
    public Date replace(JSONValue json, Class target) {
        try {
            SimpleDateFormat format;
            String val = (String) json.value();
            if (val.length() <= 20) {
                format = new SimpleDateFormat(ISO2);
            } else {
                format = new SimpleDateFormat(ISO);
            }

            Date d = format.parse(val);
            return d;
        } catch (ParseException ex) {
            Logger.getLogger(JSONDateM.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }

    @Override
    public Date replace(JSON2Value json, Class<Date> target) {
        try {
            SimpleDateFormat format;
            String val = (String) json.map();
            if (val.length() <= 20) {
                format = new SimpleDateFormat(ISO2);
            } else {
                format = new SimpleDateFormat(ISO);
            }

            Date d = format.parse(val);
            return d;
        } catch (ParseException ex) {
            Logger.getLogger(JSONDateM.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }

}
