package net.siisise.json;

import java.lang.reflect.Field;
import java.lang.reflect.Type;
import java.util.List;
import javax.json.JsonValue;
import net.siisise.bind.Rebind;
import net.siisise.json.base.JSONBaseNULL;
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
        String abc = "[1,2,{\"a\":\"b\",\"c\":5}]";
        
        System.out.println(JSON.parseWrap(abc).rebind(JSONValue.NOBR));
        
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

        System.out.println((Object)JSONBaseNULL.NULL.typeMap(foval.getType()));
        System.out.println(JSONBaseNULL.NULL.typeMap(fval.getType()).getClass().getName());

        assertNull(JSONBaseNULL.NULL.typeMap(foval.getType()));
        assertNull(JSONBaseNULL.NULL.typeMap(fintval.getType()));
        assertEquals(JsonValue.NULL, JSONBaseNULL.NULL.typeMap(fval.getType()));
        assertEquals(JSONBaseNULL.NULL, JSONBaseNULL.NULL.typeMap(fj2val.getType()));
        assertEquals(JSONBaseNULL.NULL, JSONBaseNULL.NULL.typeMap(fn2val.getType()));
        JSONBaseNULL.NULL.typeMap(fn2val.getClass());
    }
    
    @Test
    public void testRebind() {
        int[] a = {1,2,3};
        int[][] b = {a};
        String exa = "[1,2,3]";
        String exb = "[[1,2,3]]";
        
        String ra = Rebind.valueOf(a, JSON.NOBR);
        String rb = Rebind.valueOf(b, JSON.NOBR);
        assertEquals(exa,ra);
        assertEquals(exb,rb);
        
    }
}
