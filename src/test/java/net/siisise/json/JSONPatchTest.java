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
    @org.junit.jupiter.api.Test
    public void testRun() {
        System.out.println("Test");
        JSON2Object obj = (JSON2Object) JSON2.parse("{ \"foo\":\"\"}");
        JSON2Array patchList = (JSON2Array) JSON8259Reg.parse("[\r\n{ \"op\": \"add\", \"path\": \"/baz\", \"value\": \"qux\" }]");
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
        JSON2Object obj = (JSON2Object) JSON2.parseWrap("{ \"foo\":\"bar\"}");
        JSON2Array patchList = (JSON2Array) JSON2.parseWrap("[\r\n{ \"op\": \"add\", \"path\": \"/baz\", \"value\": \"qux\" }]");
        JSON2Value ex = JSON2.parseWrap("{\"foo\":\"bar\",\"baz\":\"qux\"}");
        JSONPatch p = new JSONPatch(patchList);
        JSON2Value val = p.apply(obj);
        System.out.println("src: " + obj.toJSON());
        System.out.println("diff: " + patchList.toJSON());
        System.out.println("result: "  + val.toJSON());
        assertEquals(val,ex);
        JSONPatch diff = JSONPatch.diff(obj, val);
        System.out.println("A.1.diff " + diff.toJSON().toJSON());
    }

    @org.junit.jupiter.api.Test
    public void testRunA2() {
        System.out.println("A.2.");
        JSON2Object obj = (JSON2Object) JSON8259Reg.parse("{ \"foo\": [ \"bar\", \"baz\" ] }");
        JSON2Array patchList = (JSON2Array) JSON8259Reg.parse("[\r\n { \"op\": \"add\", \"path\": \"/foo/1\", \"value\": \"qux\" }\r\n]");
        JSON2Value ex = JSON2.parseWrap("{\"foo\": [\"bar\",\"qux\",\"baz\"]}");
//        JSONPatch.run(obj, patchList);
        JSONPatch p = new JSONPatch(patchList);
        JSON2Value val = p.apply(obj);
        System.out.println("src: " + obj);
        System.out.println("diff: " + patchList.toJSON());
        System.out.println("result: " + val);
        assertEquals(val,ex);
        JSONPatch diff = JSONPatch.diff(obj, val);
        System.out.println("A.2.diff " + diff.toJSON().toJSON());
    }

    @org.junit.jupiter.api.Test
    public void testRunA3() {
        System.out.println("A.3.");
        JSON2Object obj = (JSON2Object) JSON2.parseWrap("{  \"baz\": \"qux\",\n  \"foo\": \"bar\" }");
        JSON2Array patchList = (JSON2Array) JSON2.parseWrap("[  { \"op\": \"remove\", \"path\": \"/baz\" }]");
        JSON2Value ex = JSON2.parseWrap("{\"foo\": \"bar\"}");
//        JSONPatch.run(obj, patchList);
        JSONPatch p = new JSONPatch(patchList);
        JSON2Value val = p.apply(obj);
        System.out.println("src: " + obj);
        System.out.println("diff: " + patchList.toJSON());
        System.out.println("result: " + val);
        assertEquals(val,ex);
        JSONPatch diff = JSONPatch.diff(obj, val);
        System.out.println("A.3.diff " + diff.toJSON().toJSON());
    }

    @org.junit.jupiter.api.Test
    public void testRunA4() {
        System.out.println("A.4.");
        JSON2Object<?> obj = (JSON2Object) JSON8259Reg.parse("{ \"foo\": [ \"bar\", \"qux\", \"baz\" ] }");
        JSON2Array patchList = (JSON2Array) JSON8259Reg.parse("[  { \"op\": \"remove\", \"path\": \"/foo/1\" }]");
        JSON2Value ex = JSON2.parseWrap("{\"foo\": [\"bar\",\"baz\"]}");
        JSONPatch p = new JSONPatch(patchList);
        JSON2Value val = p.apply(obj);
        System.out.println("src: " + obj);
        System.out.println("diff: " + patchList.toJSON());
        System.out.println("result: " + val);
        assertEquals(val,ex);
        JSONPatch diff = JSONPatch.diff(obj, val);
        System.out.println("A.4.diff " + diff.toJSON().toJSON());
    }

    @org.junit.jupiter.api.Test
    public void testRunA5() {
        System.out.println("A.5.");
        JSON2Object obj = (JSON2Object) JSON8259Reg.parse("{  \"baz\": \"qux\",  \"foo\": \"bar\"}");
        JSON2Array patchList = (JSON2Array) JSON8259Reg.parse("[  { \"op\": \"replace\", \"path\": \"/baz\", \"value\": \"boo\" }]");
        JSON2Value ex = JSON2.parseWrap("{\"baz\": \"boo\", \"foo\": \"bar\"}");
        JSONPatch p = new JSONPatch(patchList);
        JSON2Value val = p.apply(obj);
        System.out.println("src: " + obj);
        System.out.println("diff: " + patchList.toJSON());
        System.out.println("result: " + val);
        assertEquals(val,ex);
        JSONPatch diff = JSONPatch.diff(obj, val);
        System.out.println("A.5.diff " + diff.toJSON().toJSON());
    }

    @org.junit.jupiter.api.Test
    public void testRunA6() {
        System.out.println("A.6.");
        JSON2Object obj = (JSON2Object) JSON8259Reg.parse("{\r\n"
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
        JSON2Array patchList = (JSON2Array) JSON8259Reg.parse(patch);
        JSON2Value ex = JSON2.parseWrap("{\"foo\": { \"bar\": \"baz\"}, \"qux\": { \"corge\": \"grault\", \"thud\": \"fred\"}}");
        JSONPatch p = new JSONPatch(patchList);
        JSON2Value val = p.apply(obj);
        System.out.println("src: " + obj);
        System.out.println("diff: " + patchList.toJSON());
        System.out.println("result: " + val);
        assertEquals(val,ex);
        JSONPatch diff = JSONPatch.diff(obj, val);
        System.out.println("A.6.diff " + diff.toJSON().toJSON());
    }
}
