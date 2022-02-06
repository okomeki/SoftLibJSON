package net.siisise.json;

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
public class JSONTest {

    public JSONTest() {
    }

    @Test
    public void testValue() {
        Map obj = new HashMap();
        String abc = "[1,2,{\"a\":\"b\",\"c\":5}]";
        
        System.out.println(JSON.parseWrap(abc).toJSON(JSONValue.NOBR));
        
        List o = (JSONArray) JSON.parse(abc);
        for ( Object v : o ) {
            System.out.println(v.getClass().getName());
            System.out.println(v);
        }
        System.out.println(o);
    }

    Object oval;
    Integer intval;
    JsonValue val;
    JSONValue j2val;
    JSONNULL n2val;
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
        
        System.out.println(JSONNULL.NULL.typeMap(foval.getType()));
        System.out.println(JSONNULL.NULL.typeMap(fval.getType()).getClass().getName());
        
        assertNull(JSONNULL.NULL.typeMap(foval.getType()));
        assertNull(JSONNULL.NULL.typeMap(fintval.getType()));
        assertEquals(JSONNULL.NULL.typeMap(fval.getType()), JsonValue.NULL);
        assertEquals(JSONNULL.NULL.typeMap(fj2val.getType()), JSONNULL.NULL);
        assertEquals(JSONNULL.NULL.typeMap(fn2val.getType()), JSONNULL.NULL);
        JSONNULL.NULL.typeMap(fn2val.getClass());
    }
}
