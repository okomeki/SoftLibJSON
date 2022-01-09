package net.siisise.json.stream;

import net.siisise.json.stream.JSONPParserFactory;
import java.io.StringReader;
import java.math.BigDecimal;
import java.util.Map;
import java.util.stream.Stream;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;
import javax.json.stream.JsonLocation;
import javax.json.stream.JsonParser;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

/**
 *
 */
public class JSONPParserTest {
    
    public JSONPParserTest() {
    }

    /**
     * Test of hasNext method, of class SLJsonParser.
     */
    @Test
    public void testHasNext() {
        System.out.println("hasNext");
        String json = "{\"a\": 123, \"b\": [2,\"s\"]}";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        boolean expResult = true;
        boolean result = instance.hasNext();
        assertEquals(expResult, result);
    }

    /**
     * Test of next method, of class SLJsonParser.
     */
    @Test
    public void testNext() {
        System.out.println("next");
        String json = "{\"a\": 123, \"b\": [2,\"s\"]}";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        JsonParser.Event expResult = JsonParser.Event.START_OBJECT;
        JsonParser.Event result = instance.next();
        assertEquals(expResult, result);
        assertEquals(JsonParser.Event.KEY_NAME,instance.next());
        assertEquals(JsonParser.Event.VALUE_NUMBER,instance.next());
        assertEquals(JsonParser.Event.KEY_NAME,instance.next());
        assertEquals(JsonParser.Event.START_ARRAY,instance.next());
    }

    /**
     * Test of getString method, of class SLJsonParser.
     */
    @Test
    public void testGetString() {
        System.out.println("getString");
        String json = "{\"a\": \"123\", \"b\": [2,\"s\"]}";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        String expResult = "123";
        instance.next();
        instance.next();
        instance.next();
        String result = instance.getString();
        assertEquals(expResult, result);
    }

    /**
     * Test of isIntegralNumber method, of class SLJsonParser.
     */
    @Test
    public void testIsIntegralNumber() {
        System.out.println("isIntegralNumber");
        String json = "{\"a\": \"123\", \"b\": [2,\"s\"]}";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        instance.next();
        instance.next();
        instance.next();
        boolean expResult = false;
        boolean result = instance.isIntegralNumber();
        assertEquals(expResult, result);
    }

    /**
     * Test of getInt method, of class SLJsonParser.
     */
    @Test
    public void testGetInt() {
        System.out.println("getInt");
        String json = "{\"a\": \"123\", \"b\": [2,\"s\"]}";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        instance.next();
        instance.next();
        instance.next();
        instance.next();
        instance.next();
        instance.next();
        int expResult = 2;
        int result = instance.getInt();
        assertEquals(expResult, result);
    }

    /**
     * Test of getLong method, of class SLJsonParser.
     */
    @Test
    public void testGetLong() {
        System.out.println("getLong");
        String json = "{\"a\": \"123\", \"b\": [2,\"s\"]}";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        instance.next();
        instance.next();
        instance.next();
        instance.next();
        instance.next();
        instance.next();
        long expResult = 2L;
        long result = instance.getLong();
        assertEquals(expResult, result);
    }

    /**
     * Test of getBigDecimal method, of class SLJsonParser.
     */
    @Test
    public void testGetBigDecimal() {
        System.out.println("getBigDecimal");
        String json = "{\"a\": \"123\", \"b\": [2,\"s\"]}";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        instance.next();
        instance.next();
        instance.next();
        instance.next();
        instance.next();
        instance.next();
        BigDecimal expResult = new BigDecimal(2);
        BigDecimal result = instance.getBigDecimal();
        assertEquals(expResult, result);
    }

    /**
     * Test of getLocation method, of class SLJsonParser.
     */
    @Test
    public void testGetLocation() {
        System.out.println("getLocation");
        String json = "{\"a\": \"123\", \"b\": [2,\"s\"]}";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        JsonLocation expResult = null;
        JsonLocation result = instance.getLocation();
//        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
    }

    /**
     * Test of getObject method, of class SLJsonParser.
     */
    @Test
    public void testGetObject() {
        System.out.println("getObject");
        String json = "{\"a\": \"123\", \"b\": [2,\"s\"]}";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        instance.next();
        JsonObject result = instance.getObject();
        assertNotNull(result);
        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
    }

    /**
     * Test of getValue method, of class SLJsonParser.
     */
    @Test
    public void testGetValue() {
        System.out.println("getValue");
        String json = "{\"a\": \"123\", \"b\": [2,\"s\"]}";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        String expResult = "123";
        instance.next();
        JsonObject result = (JsonObject) instance.getValue();
        assertNotNull(expResult,result.getString("a"));
    }

    /**
     * Test of getArray method, of class SLJsonParser.
     */
    @Test
    public void testGetArray() {
        System.out.println("getArray");
        String json = "[{\"a\": \"123\", \"b\": [2,\"s\"]},3,5]";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        JsonArray expResult = null;
        instance.next();
        JsonArray result = instance.getArray();
        assertNotNull(result);
    }

    /**
     * Test of getArrayStream method, of class SLJsonParser.
     */
    @Test
    public void testGetArrayStream() {
        System.out.println("getArrayStream");
        String json = "[[1,2,3],[4,6,7,8]]";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        int expResult = 3;
        instance.next();
        instance.next();
        Stream<JsonValue> result = instance.getArrayStream();
        assertEquals(expResult, result.count());
    }

    /**
     * Test of getObjectStream method, of class SLJsonParser.
     */
    @Test
    public void testGetObjectStream() {
        System.out.println("getObjectStream");
        String json = "{\"a\": \"123\", \"b\": [2,\"s\"]}";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        int expResult = 2;
        instance.next();
        Stream<Map.Entry<String, JsonValue>> result = instance.getObjectStream();
        assertEquals(expResult, result.count());
    }

    /**
     * Test of getValueStream method, of class SLJsonParser.
     */
    @Test
    public void testGetValueStream() {
        System.out.println("getValueStream");
        String json = "{\"a\": \"123\", \"b\": [2,\"s\"]}[4,5]{}4";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        int expResult = 4;
        instance.next();
        Stream<JsonValue> result = instance.getValueStream();
        assertEquals(expResult, result.count());

    }

    /**
     * Test of close method, of class SLJsonParser.
     */
    @Test
    public void testClose() {
        System.out.println("close");
        String json = "{\"a\": \"123\", \"b\": [2,\"s\"]}";
        JsonParser instance = new JSONPParserFactory().createParser(new StringReader(json));
        instance.next();
        instance.close();
    }
    
}
