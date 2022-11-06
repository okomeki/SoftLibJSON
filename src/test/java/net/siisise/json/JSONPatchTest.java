package net.siisise.json;

import net.siisise.json.parser.JSON8259Reg;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.AfterAll;
import static org.junit.jupiter.api.Assertions.assertEquals;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

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
    @Test
    public void testRun() {
        System.out.println("Test");
        JSONObject obj = (JSONObject) JSON.parse("{ \"foo\":\"\"}");
        JSONArray patchList = (JSONArray) JSON8259Reg.parse("[\r\n{ \"op\": \"add\", \"path\": \"/baz\", \"value\": \"qux\" }]");
        JSONPatch.run(obj, patchList);
        System.out.println("A.1...." + obj);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }

    /**
     * Test of run method, of class JSONPatch.
     */
    @Test
    public void testRunA1() {
        System.out.println("A.1.");
        JSONObject obj = (JSONObject) JSON.parseWrap("{ \"foo\":\"bar\"}");
        JSONArray patchList = (JSONArray) JSON.parseWrap("[\r\n{ \"op\": \"add\", \"path\": \"/baz\", \"value\": \"qux\" }]");
        JSONValue ex = JSON.parseWrap("{\"foo\":\"bar\",\"baz\":\"qux\"}");
        JSONPatch p = new JSONPatch(patchList);
        JSONValue val = p.apply(obj);
        System.out.println("src: " + obj.toJSON());
        System.out.println("diff: " + patchList.toJSON());
        System.out.println("result: "  + val.toJSON());
        assertEquals(val,ex);
        JSONPatch diff = JSONPatch.diff(obj, val);
        System.out.println("A.1.diff " + diff.toJSON().toJSON());
    }

    @Test
    public void testRunA2() {
        System.out.println("A.2.");
        JSONObject obj = (JSONObject) JSON8259Reg.parse("{ \"foo\": [ \"bar\", \"baz\" ] }");
        JSONArray patchList = (JSONArray) JSON8259Reg.parse("[\r\n { \"op\": \"add\", \"path\": \"/foo/1\", \"value\": \"qux\" }\r\n]");
        JSONValue ex = JSON.parseWrap("{\"foo\": [\"bar\",\"qux\",\"baz\"]}");
//        JSONPatch.run(obj, patchList);
        JSONPatch p = new JSONPatch(patchList);
        JSONValue val = p.apply(obj);
        System.out.println("src: " + obj);
        System.out.println("diff: " + patchList.toJSON());
        System.out.println("result: " + val);
        assertEquals(val,ex);
        JSONPatch diff = JSONPatch.diff(obj, val);
        System.out.println("A.2.diff " + diff.toJSON().toJSON());
    }

    @Test
    public void testRunA3() {
        System.out.println("A.3.");
        JSONObject obj = (JSONObject) JSON.parseWrap("{  \"baz\": \"qux\",\n  \"foo\": \"bar\" }");
        JSONArray patchList = (JSONArray) JSON.parseWrap("[  { \"op\": \"remove\", \"path\": \"/baz\" }]");
        JSONValue ex = JSON.parseWrap("{\"foo\": \"bar\"}");
//        JSONPatch.run(obj, patchList);
        JSONPatch p = new JSONPatch(patchList);
        JSONValue val = p.apply(obj);
        System.out.println("src: " + obj);
        System.out.println("diff: " + patchList.toJSON());
        System.out.println("result: " + val);
        assertEquals(val,ex);
        JSONPatch diff = JSONPatch.diff(obj, val);
        System.out.println("A.3.diff " + diff.toJSON().toJSON());
    }

    @Test
    public void testRunA4() {
        System.out.println("A.4.");
        JSONObject<?> obj = (JSONObject) JSON8259Reg.parse("{ \"foo\": [ \"bar\", \"qux\", \"baz\" ] }");
        JSONArray patchList = (JSONArray) JSON8259Reg.parse("[  { \"op\": \"remove\", \"path\": \"/foo/1\" }]");
        JSONValue ex = JSON.parseWrap("{\"foo\": [\"bar\",\"baz\"]}");
        JSONPatch p = new JSONPatch(patchList);
        JSONValue val = p.apply(obj);
        System.out.println("src: " + obj);
        System.out.println("diff: " + patchList.toJSON());
        System.out.println("result: " + val);
        assertEquals(val,ex);
        JSONPatch diff = JSONPatch.diff(obj, val);
        System.out.println("A.4.diff " + diff.toJSON().toJSON());
    }

    @Test
    public void testRunA5() {
        System.out.println("A.5.");
        JSONObject obj = (JSONObject) JSON8259Reg.parse("{  \"baz\": \"qux\",  \"foo\": \"bar\"}");
        JSONArray patchList = (JSONArray) JSON8259Reg.parse("[  { \"op\": \"replace\", \"path\": \"/baz\", \"value\": \"boo\" }]");
        JSONValue ex = JSON.parseWrap("{\"baz\": \"boo\", \"foo\": \"bar\"}");
        JSONPatch p = new JSONPatch(patchList);
        JSONValue val = p.apply(obj);
        System.out.println("src: " + obj);
        System.out.println("diff: " + patchList.toJSON());
        System.out.println("result: " + val);
        assertEquals(val,ex);
        JSONPatch diff = JSONPatch.diff(obj, val);
        System.out.println("A.5.diff " + diff.toJSON().toJSON());
    }

    @Test
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
        JSONArray patchList = (JSONArray) JSON8259Reg.parse(patch);
        JSONValue ex = JSON.parseWrap("{\"foo\": { \"bar\": \"baz\"}, \"qux\": { \"corge\": \"grault\", \"thud\": \"fred\"}}");
        JSONPatch p = new JSONPatch(patchList);
        JSONValue val = p.apply(obj);
        System.out.println("src: " + obj);
        System.out.println("diff: " + patchList.toJSON());
        System.out.println("result: " + val);
        assertEquals(val,ex);
        JSONPatch diff = JSONPatch.diff(obj, val);
        System.out.println("A.6.diff " + diff.toJSON().toJSON());
    }
}
