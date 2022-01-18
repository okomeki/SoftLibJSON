package net.siisise.json.mergepatch;

import net.siisise.json.JSONMergePatch7396;
import net.siisise.json.JSON2;
import net.siisise.json.JSON2Value;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

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
        JSON2Value original;
        JSON2Value patch;
        JSON2Value expResult;
        JSON2Value result;
        
        original = JSON2.parseWrap("{\"a\":\"b\"}");
        patch = JSON2.parseWrap("{\"a\":\"c\"}");
        expResult = JSON2.parseWrap("{\"a\":\"c\"}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON2.parseWrap("{\"a\":\"b\"}");
        patch = JSON2.parseWrap("{\"b\":\"c\"}");
        expResult = JSON2.parseWrap("{\"a\":\"b\",\"b\":\"c\"}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON2.parseWrap("{\"a\":\"b\"}");
        patch = JSON2.parseWrap("{\"a\":null}");
        expResult = JSON2.parseWrap("{}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON2.parseWrap("{\"a\":\"b\",\"b\":\"c\"}");
        patch = JSON2.parseWrap("{\"a\":null}");
        expResult = JSON2.parseWrap("{\"b\":\"c\"}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON2.parseWrap("{\"a\":[\"b\"]}");
        patch = JSON2.parseWrap("{\"a\":\"c\"}");
        expResult = JSON2.parseWrap("{\"a\":\"c\"}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON2.parseWrap("{\"a\":\"c\"}");
        patch = JSON2.parseWrap("{\"a\":[\"b\"]}");
        expResult = JSON2.parseWrap("{\"a\":[\"b\"]}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON2.parseWrap("{\"a\": {\"b\":\"c\"}}");
        patch = JSON2.parseWrap("{\"a\": {\"b\": \"d\", \"c\": null}}");
        expResult = JSON2.parseWrap("{\"a\": { \"b\": \"d\" }}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON2.parseWrap("{\"a\":[ {\"b\":\"c\"}]}");
        patch = JSON2.parseWrap("{\"a\": [1]}");
        expResult = JSON2.parseWrap("{\"a\": [1]}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON2.parseWrap("[\"a\",\"b\"]");
        patch = JSON2.parseWrap("[\"c\",\"d\"]");
        expResult = JSON2.parseWrap("[\"c\",\"d\"]");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON2.parseWrap("{\"a\":\"b\"}");
        patch = JSON2.parseWrap("[\"c\"]");
        expResult = JSON2.parseWrap("[\"c\"]");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON2.parseWrap("{\"a\":\"foo\"}");
        patch = JSON2.parseWrap("null");
        expResult = JSON2.parseWrap("null");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON2.parseWrap("{\"a\":\"foo\"}");
        patch = JSON2.parseWrap("\"bar\"");
        expResult = JSON2.parseWrap("\"bar\"");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON2.parseWrap("{\"e\":null}");
        patch = JSON2.parseWrap("{\"a\":1}");
        expResult = JSON2.parseWrap("{\"e\":null,\"a\":1}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON2.parseWrap("[1,2]");
        patch = JSON2.parseWrap("{\"a\":\"b\",\"c\":null}");
        expResult = JSON2.parseWrap("{\"a\":\"b\"}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);

        original = JSON2.parseWrap("{}");
        patch = JSON2.parseWrap("{\"a\":{\"bb\":{\"ccc\":null}}}");
        expResult = JSON2.parseWrap("{\"a\":{\"bb\":{}}}");
        result = JSONMergePatch7396.mergePatch(original, patch);
        System.out.println(result);
        assertEquals(expResult, result);
    }
    
}
