package net.siisise.json.map;

import java.lang.reflect.Type;
import java.text.SimpleDateFormat;
import java.util.Date;
import net.siisise.bind.TypeUnbind;
import net.siisise.bind.format.TypeFormat;

public class JSONDateM implements TypeUnbind {

//    static String ISO = "yyyy-MM-dd'T'HH:mm:ss.SSSX";
    static String ISO2 = "yyyy-MM-dd'T'HH:mm:ssX";


    @Override
    public Type[] getSrcTypes() {
        return new Type[] { Date.class };
    }
    /**
     *
     * @param obj
     * @param typeFormat
     * @return
     */
    @Override
    public <T> T valueOf(Object obj, TypeFormat<T> typeFormat) {
        if (obj instanceof Date) { //&& typeFormat.targetClass() == String.class) {
            SimpleDateFormat dateFormat = new SimpleDateFormat(ISO2);
            String s = dateFormat.format((Date) obj);
            return typeFormat.stringFormat(s);
        }
        return (T) this;
    }

}
