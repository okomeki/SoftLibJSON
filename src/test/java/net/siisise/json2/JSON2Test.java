package net.siisise.json2;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import net.siisise.json.JSON;
import org.junit.jupiter.api.Test;

/**
 *
 */
public class JSON2Test {

    public JSON2Test() {
    }

    @Test
    public void testValue() {
        Map obj = new HashMap();
        String abc = "[1,2,{\"a\":\"b\",\"c\":5}]";
        
        System.out.println(JSON.parse(abc).toString(JSON.NOBR));
        
        List o = (JSON2Array) JSON2.parse(abc);
        for ( Object v : o ) {
            System.out.println(v.getClass().getName());
            System.out.println(v);
        }
        System.out.println(o);
    }
}
