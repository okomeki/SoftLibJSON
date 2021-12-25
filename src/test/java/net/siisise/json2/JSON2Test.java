package net.siisise.json2;

import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.json.JsonValue;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

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
        
        System.out.println(JSON2.parseWrap(abc).toString(JSON2Value.NOBR));
        
        List o = (JSON2Array) JSON2.parse(abc);
        for ( Object v : o ) {
            System.out.println(v.getClass().getName());
            System.out.println(v);
        }
        System.out.println(o);
    }

    Object oval;
    Integer intval;
    JsonValue val;
    JSON2Value j2val;
    JSON2NULL n2val;
    List<? extends Integer> lint;
    Integer[] aint;
    List<Integer>[] laint;

    @Test
    public void testNullMap() throws NoSuchFieldException {
        Field foval = getClass().getDeclaredField("oval");
        Field fintval = getClass().getDeclaredField("intval");
        Field fval = getClass().getDeclaredField("val");
        Field fj2val = getClass().getDeclaredField("j2val");
        Field fn2val = getClass().getDeclaredField("n2val");
        
        Field lint = getClass().getDeclaredField("lint");
        Field aint = getClass().getDeclaredField("aint");
        
        Type lintt = lint.getGenericType();
        Type aintt = aint.getGenericType();
        
        System.out.println("type test");
        System.out.println(lintt.getClass().getName());
        System.out.println(lintt.getClass().isArray());
        System.out.println(lintt.getTypeName());
//        System.out.println(lintt.getType().getClass().getName());
        System.out.println(aintt.getClass().getName()); // 配列
        System.out.println(aintt.getClass().isArray()); // 配列
        System.out.println(aintt.getTypeName());
//        System.out.println(aint.getType().getClass().getName());
        
        System.out.println(JSON2NULL.NULL.typeMap(foval.getType()));
        System.out.println(JSON2NULL.NULL.typeMap(fval.getType()).getClass().getName());
        
        assertNull(JSON2NULL.NULL.typeMap(foval.getType()));
        assertNull(JSON2NULL.NULL.typeMap(fintval.getType()));
        assertEquals(JSON2NULL.NULL.typeMap(fval.getType()), JsonValue.NULL);
        assertEquals(JSON2NULL.NULL.typeMap(fj2val.getType()), JSON2NULL.NULL);
        assertEquals(JSON2NULL.NULL.typeMap(fn2val.getType()), JSON2NULL.NULL);
        JSON2NULL.NULL.typeMap(fn2val.getClass());
    }
}
