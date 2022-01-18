package net.siisise.json.pointer;

import net.siisise.json.JSONPatch;
import net.siisise.json.JSON2;
import net.siisise.json.JSON28259Reg;
import net.siisise.json.JSON2Array;
import net.siisise.json.JSON2Collection;
import net.siisise.json.JSON2Object;
import net.siisise.json.JSON2Value;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.AfterAll;
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
        JSON2Array patchList = (JSON2Array) JSON28259Reg.parse("[\r\n{ \"op\": \"add\", \"path\": \"/baz\", \"value\": \"qux\" }]");
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
        JSON2Object obj = (JSON2Object) JSON28259Reg.parse("{ \"foo\":\"bar\"}");
        JSON2Array patchList = (JSON2Array) JSON28259Reg.parse("[\r\n{ \"op\": \"add\", \"path\": \"/baz\", \"value\": \"qux\" }]");
        JSONPatch.run(obj, patchList);
        System.out.println("A.1...." + obj);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }

    @org.junit.jupiter.api.Test
    public void testRunA2() {
        System.out.println("A.2.");
        JSON2Object obj = (JSON2Object) JSON28259Reg.parse("{ \"foo\": [ \"bar\", \"baz\" ] }");
        JSON2Array patchList = (JSON2Array) JSON28259Reg.parse("[\r\n { \"op\": \"add\", \"path\": \"/foo/1\", \"value\": \"qux\" }\r\n]");
//        JSONPatch.run(obj, patchList);
        JSONPatch p = new JSONPatch(patchList);
        obj = p.apply(obj);
        System.out.println("A.2...." + obj);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }

    @org.junit.jupiter.api.Test
    public void testRunA3() {
        System.out.println("A.3.");
        JSON2Object obj = (JSON2Object) JSON28259Reg.parse("{  \"baz\": \"qux\",\n  \"foo\": \"bar\" }");
        JSON2Array patchList = (JSON2Array) JSON28259Reg.parse("[  { \"op\": \"remove\", \"path\": \"/baz\" }]");
//        JSONPatch.run(obj, patchList);
        JSONPatch p = new JSONPatch(patchList);
        obj = p.apply(obj);
        System.out.println("A.3...." + obj);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }

    @org.junit.jupiter.api.Test
    public void testRunA4() {
        System.out.println("A.4.");
        JSON2Object<?> obj = (JSON2Object) JSON28259Reg.parse("{ \"foo\": [ \"bar\", \"qux\", \"baz\" ] }");
        JSON2Array patchList = (JSON2Array) JSON28259Reg.parse("[  { \"op\": \"remove\", \"path\": \"/foo/1\" }]");
        JSONPatch p = new JSONPatch(patchList);
        obj = p.apply(obj);
        System.out.println("A.4...." + obj);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }

    @org.junit.jupiter.api.Test
    public void testRunA5() {
        System.out.println("A.5.");
        JSON2Object obj = (JSON2Object) JSON28259Reg.parse("{  \"baz\": \"qux\",  \"foo\": \"bar\"}");
        JSON2Array patchList = (JSON2Array) JSON28259Reg.parse("[  { \"op\": \"replace\", \"path\": \"/baz\", \"value\": \"boo\" }]");
        
        JSONPatch p = new JSONPatch(patchList);
        p.toJsonArray();
        
        obj = (JSON2Object)p.apply(obj);
        //JSONPatch.run(obj, patchList);
        System.out.println("A.5...." + obj);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }

    @org.junit.jupiter.api.Test
    public void testRunA6() {
        System.out.println("A.6.");
        JSON2Object obj = (JSON2Object) JSON28259Reg.parse("{\r\n"
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
        System.out.println("src:");
        System.out.println(obj.toString());
//        Object string = JSON8259Reg.REG.parse("string", "\"op\"");
//        JSONValue v1 = JSON8259Reg.REG.parse("object",pa);
        JSON2Array patchList = (JSON2Array) JSON28259Reg.parse(patch);
        System.out.println("JSON patch:");
        System.out.println(patchList.toString());
        JSONPatch p = new JSONPatch(patchList);
        JSON2Value obj2 = p.apply(obj);
//        JSONPatch.run(obj, patchList);
        System.out.println("A.6...." + obj2.toString());
        p = JSONPatch.diff(obj, obj2);
        System.out.println("diff");
        System.out.println(p.toJSON().toString());
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }
}
