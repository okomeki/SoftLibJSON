package net.siisise.json.pointer;

import net.siisise.json.JSON8259Reg;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONObject;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.BeforeAll;

/**
 *
 */
public class JSONPatchTest {

    public JSONPatchTest() {
    }

    @BeforeAll
    public static void setUpClass() {
    }

    @AfterAll
    public static void tearDownClass() {
    }

    @BeforeEach
    public void setUp() {
    }

    @AfterEach
    public void tearDown() {
    }

    /**
     * Test of run method, of class JSONPatch.
     */
    @org.junit.jupiter.api.Test
    public void testRun() {
        System.out.println("Test");
        JSONObject obj = (JSONObject) JSON8259Reg.parse("{ \"foo\":\"\"}");
        JSONArray patchList = (JSONArray) JSON8259Reg.parse("[\r\n{ \"op\": \"add\", \"path\": \"/baz\", \"value\": \"qux\" }]");
        JSONPatch.run(obj, patchList);
        System.out.println("A.1...." + obj);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }

    /**
     * Test of run method, of class JSONPatch.
     */
    @org.junit.jupiter.api.Test
    public void testRunA1() {
        System.out.println("A.1.");
        JSONObject obj = (JSONObject) JSON8259Reg.parse("{ \"foo\":\"bar\"}");
        JSONArray patchList = (JSONArray) JSON8259Reg.parse("[\r\n{ \"op\": \"add\", \"path\": \"/baz\", \"value\": \"qux\" }]");
        JSONPatch.run(obj, patchList);
        System.out.println("A.1...." + obj);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }

    @org.junit.jupiter.api.Test
    public void testRunA2() {
        System.out.println("A.2.");
        JSONObject obj = (JSONObject) JSON8259Reg.parse("{ \"foo\": [ \"bar\", \"baz\" ] }");
        JSONArray patchList = (JSONArray) JSON8259Reg.parse("[\r\n { \"op\": \"add\", \"path\": \"/foo/1\", \"value\": \"qux\" }\r\n]");
        JSONPatch.run(obj, patchList);
        System.out.println("A.2...." + obj);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }

    @org.junit.jupiter.api.Test
    public void testRunA3() {
        System.out.println("A.3.");
        JSONObject obj = (JSONObject) JSON8259Reg.parse("{  \"baz\": \"qux\",\n  \"foo\": \"bar\" }");
        JSONArray patchList = (JSONArray) JSON8259Reg.parse("[  { \"op\": \"remove\", \"path\": \"/baz\" }]");
        JSONPatch.run(obj, patchList);
        System.out.println("A.3...." + obj);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }

    @org.junit.jupiter.api.Test
    public void testRunA4() {
        System.out.println("A.4.");
        JSONObject obj = (JSONObject) JSON8259Reg.parse("{ \"foo\": [ \"bar\", \"qux\", \"baz\" ] }");
        JSONArray patchList = (JSONArray) JSON8259Reg.parse("[  { \"op\": \"remove\", \"path\": \"/foo/1\" }]");
        JSONPatch.run(obj, patchList);
        System.out.println("A.4...." + obj);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }

    @org.junit.jupiter.api.Test
    public void testRunA5() {
        System.out.println("A.5.");
        JSONObject obj = (JSONObject) JSON8259Reg.parse("{  \"baz\": \"qux\",  \"foo\": \"bar\"}");
        JSONArray patchList = (JSONArray) JSON8259Reg.parse("[  { \"op\": \"replace\", \"path\": \"/baz\", \"value\": \"boo\" }]");
        JSONPatch.run(obj, patchList);
        System.out.println("A.5...." + obj);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }

    @org.junit.jupiter.api.Test
    public void testRunA6() {
        System.out.println("A.6.");
        JSONObject obj = (JSONObject) JSON8259Reg.parse("{\r\n"
                + "  \"foo\": {"
                + "    \"bar\": \"baz\","
                + "    \"waldo\": \"fred\""
                + "  },"
                + "  \"qux\": {"
                + "    \"corge\": \"grault\""
                + "  }"
                + "}");
//        String pa = "{ \"op\": \"move\" }";
        String patch = "[\r\n"
                + "  { \"op\": \"move\", \"from\": \"/foo/waldo\", \"path\": \"/qux/thud\" }\r\n"
                + "]";
        
//        Object string = JSON8259Reg.REG.parse("string", "\"op\"");
//        JSONValue v1 = JSON8259Reg.REG.parse("object",pa);

        JSONArray patchList = (JSONArray) JSON8259Reg.parse(patch);
        JSONPatch.run(obj, patchList);
        System.out.println("A.6...." + obj);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }
}
