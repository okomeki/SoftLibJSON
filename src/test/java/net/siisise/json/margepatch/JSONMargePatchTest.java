package net.siisise.json.margepatch;

import net.siisise.json.mergepatch.JSONMergePatch;
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
     * Test of mergePatch method, of class JSONMergePatch.
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
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{\"a\":\"b\"}");
        patch = JSON.parse("{\"b\":\"c\"}");
        expResult = JSON.parse("{\"a\":\"b\",\"b\":\"c\"}");
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{\"a\":\"b\"}");
        patch = JSON.parse("{\"a\":null}");
        expResult = JSON.parse("{}");
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{\"a\":\"b\",\"b\":\"c\"}");
        patch = JSON.parse("{\"a\":null}");
        expResult = JSON.parse("{\"b\":\"c\"}");
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{\"a\":[\"b\"]}");
        patch = JSON.parse("{\"a\":\"c\"}");
        expResult = JSON.parse("{\"a\":\"c\"}");
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{\"a\":\"c\"}");
        patch = JSON.parse("{\"a\":[\"b\"]}");
        expResult = JSON.parse("{\"a\":[\"b\"]}");
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{\"a\": {\"b\":\"c\"}}");
        patch = JSON.parse("{\"a\": {\"b\": \"d\", \"c\": null}}");
        expResult = JSON.parse("{\"a\": { \"b\": \"d\" }}");
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{\"a\":[ {\"b\":\"c\"}]}");
        patch = JSON.parse("{\"a\": [1]}");
        expResult = JSON.parse("{\"a\": [1]}");
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("[\"a\",\"b\"]");
        patch = JSON.parse("[\"c\",\"d\"]");
        expResult = JSON.parse("[\"c\",\"d\"]");
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{\"a\":\"b\"}");
        patch = JSON.parse("[\"c\"]");
        expResult = JSON.parse("[\"c\"]");
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{\"a\":\"foo\"}");
        patch = JSON.parse("null");
        expResult = JSON.parse("null");
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{\"a\":\"foo\"}");
        patch = JSON.parse("\"bar\"");
        expResult = JSON.parse("\"bar\"");
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{\"e\":null}");
        patch = JSON.parse("{\"a\":1}");
        expResult = JSON.parse("{\"e\":null,\"a\":1}");
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("[1,2]");
        patch = JSON.parse("{\"a\":\"b\",\"c\":null}");
        expResult = JSON.parse("{\"a\":\"b\"}");
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parse("{}");
        patch = JSON.parse("{\"a\":{\"bb\":{\"ccc\":null}}}");
        expResult = JSON.parse("{\"a\":{\"bb\":{}}}");
        result = JSONMergePatch.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);
    }
    
}
