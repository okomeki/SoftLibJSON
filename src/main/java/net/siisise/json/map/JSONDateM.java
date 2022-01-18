package net.siisise.json.map;

import java.text.SimpleDateFormat;
import java.util.Date;
import net.siisise.json.bind.MtoConvert;
import net.siisise.json.bind.OMConvert;

public class JSONDateM implements OMConvert {

//    static String ISO = "yyyy-MM-dd'T'HH:mm:ss.SSSX";
    static String ISO2 = "yyyy-MM-dd'T'HH:mm:ssX";


    @Override
    public Class[] getSrcClasses() {
        return new Class[] { Date.class };
    }
    /**
     *
     * @return
     */
    @Override
    public <T> T valueOf(Object obj, MtoConvert<T> outConvert) {
        if (obj instanceof Date) { //&& outConvert.targetClass() == String.class) {
            SimpleDateFormat format = new SimpleDateFormat(ISO2);
            String s = format.format((Date) obj);
            return outConvert.stringValue(s);
        }
        return (T) this;
    }

}
