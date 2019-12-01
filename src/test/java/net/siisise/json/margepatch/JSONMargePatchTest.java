package net.siisise.json.margepatch;

import net.siisise.json.JSON;
import net.siisise.json.JSONValue;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

/**
 *
 * @author okome
 */
public class JSONMargePatchTest {
    
    public JSONMargePatchTest() {
    }

    /**
     * Test of mergePatch method, of class JSONMargePatch.
     */
    @Test
    public void testAppendixA() {
        System.out.println("Appendix A.");
        JSONValue original;
        JSONValue patch;
        JSONValue expResult;
        JSONValue result;
        
        original = JSON.parse("{\"a\":\"b\"}");
        patch = JSON.parse("{\"a\":\"c\"}");
        expResult = JSON.parse("{\"a\":\"c\"}");
        result = JSONMargePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{\"a\":\"b\"}");
        patch = JSON.parse("{\"b\":\"c\"}");
        expResult = JSON.parse("{\"a\":\"b\",\"b\":\"c\"}");
        result = JSONMargePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{\"a\":\"b\"}");
        patch = JSON.parse("{\"a\":null}");
        expResult = JSON.parse("{}");
        result = JSONMargePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{\"a\":\"b\",\"b\":\"c\"}");
        patch = JSON.parse("{\"a\":null}");
        expResult = JSON.parse("{\"b\":\"c\"}");
        result = JSONMargePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);
    }
    
}
