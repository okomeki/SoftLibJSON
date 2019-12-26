package net.siisise.json.map;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Level;
import java.util.logging.Logger;
import net.siisise.json.JSONReplace;
import net.siisise.json.JSONValue;

/**
 *
 * @author okome
 */
public class JSONDate implements JSONReplace<Date> {

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

    public static JSONValue valueOf(Object src) {
        if (src instanceof Date) {
            SimpleDateFormat format = new SimpleDateFormat(ISO2);
            String s = format.format((Date) src);
            return JSONValue.valueOf(s);
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
            Logger.getLogger(JSONDate.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }

}
