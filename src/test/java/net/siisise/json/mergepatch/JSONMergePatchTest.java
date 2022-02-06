package net.siisise.json.mergepatch;

import net.siisise.json.JSONMergePatch7396;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import net.siisise.json.JSON;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONMergePatchTest {
    
    public JSONMergePatchTest() {
    }

    /**
     * Test of mergePatch method, of class JSONMergePatch7396.
     */
    @Test
    public void testAppendixA() {
        System.out.println("Appendix A.");
        JSONValue original;
        JSONValue patch;
        JSONValue expResult;
        JSONValue result;
        
        original = JSON.parseWrap("{\"a\":\"b\"}");
        patch = JSON.parseWrap("{\"a\":\"c\"}");
        expResult = JSON.parseWrap("{\"a\":\"c\"}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parseWrap("{\"a\":\"b\"}");
        patch = JSON.parseWrap("{\"b\":\"c\"}");
        expResult = JSON.parseWrap("{\"a\":\"b\",\"b\":\"c\"}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parseWrap("{\"a\":\"b\"}");
        patch = JSON.parseWrap("{\"a\":null}");
        expResult = JSON.parseWrap("{}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parseWrap("{\"a\":\"b\",\"b\":\"c\"}");
        patch = JSON.parseWrap("{\"a\":null}");
        expResult = JSON.parseWrap("{\"b\":\"c\"}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parseWrap("{\"a\":[\"b\"]}");
        patch = JSON.parseWrap("{\"a\":\"c\"}");
        expResult = JSON.parseWrap("{\"a\":\"c\"}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parseWrap("{\"a\":\"c\"}");
        patch = JSON.parseWrap("{\"a\":[\"b\"]}");
        expResult = JSON.parseWrap("{\"a\":[\"b\"]}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parseWrap("{\"a\": {\"b\":\"c\"}}");
        patch = JSON.parseWrap("{\"a\": {\"b\": \"d\", \"c\": null}}");
        expResult = JSON.parseWrap("{\"a\": { \"b\": \"d\" }}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parseWrap("{\"a\":[ {\"b\":\"c\"}]}");
        patch = JSON.parseWrap("{\"a\": [1]}");
        expResult = JSON.parseWrap("{\"a\": [1]}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parseWrap("[\"a\",\"b\"]");
        patch = JSON.parseWrap("[\"c\",\"d\"]");
        expResult = JSON.parseWrap("[\"c\",\"d\"]");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parseWrap("{\"a\":\"b\"}");
        patch = JSON.parseWrap("[\"c\"]");
        expResult = JSON.parseWrap("[\"c\"]");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parseWrap("{\"a\":\"foo\"}");
        patch = JSON.parseWrap("null");
        expResult = JSON.parseWrap("null");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parseWrap("{\"a\":\"foo\"}");
        patch = JSON.parseWrap("\"bar\"");
        expResult = JSON.parseWrap("\"bar\"");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parseWrap("{\"e\":null}");
        patch = JSON.parseWrap("{\"a\":1}");
        expResult = JSON.parseWrap("{\"e\":null,\"a\":1}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parseWrap("[1,2]");
        patch = JSON.parseWrap("{\"a\":\"b\",\"c\":null}");
        expResult = JSON.parseWrap("{\"a\":\"b\"}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON.parseWrap("{}");
        patch = JSON.parseWrap("{\"a\":{\"bb\":{\"ccc\":null}}}");
        expResult = JSON.parseWrap("{\"a\":{\"bb\":{}}}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);
    }
    
}
