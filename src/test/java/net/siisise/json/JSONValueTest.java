package net.siisise.json;

import java.util.ArrayList;
import java.util.List;
import net.siisise.json2.JSON2;
import net.siisise.json2.JSON2Value;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/**
 *
 */
public class JSONValueTest {
    
    public JSONValueTest() {
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

    public static class TestB {
        public int c;
        public double d;
        public int[] e;
    }
    
    public static class TestA {
        public TestB b;
        public List<TestB> lb;
    }

    /**
     * Test of valueOf method, of class JSONValue.
     */
    @Test
    public void testValueOf() {
        System.out.println("valueOf");
        TestB bb = new TestB();
        bb.c = 7;
        bb.d = 6.7;
        bb.e = new int[] { 8,9};
        
        TestB bb2 = new TestB();
        bb2.c = 8;
        
        TestA aa = new TestA();
        aa.b = bb;
        aa.lb = new ArrayList();
        aa.lb.add(bb2);
        JSON2Value aajson = JSON2.valueOf(aa);
        System.out.println(aajson);
        TestA cc = (TestA)aajson.typeMap(TestA.class);
        TestB nb = cc.lb.get(0);
        aajson = JSON2.valueOf(cc);
        System.out.println(aajson);
//        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
    }
    
    @Test
    public void testMap() {
        JSON2Value intNumber = JSON2.valueOf(1);
        JSON2Value longNumber = JSON2.valueOf(1l);
        JSON2Value doubleNumber = JSON2.valueOf(1.1d);
        intNumber.typeMap(Long.class);
        intNumber.typeMap(Long.TYPE);
        longNumber.typeMap(Long.class);
        longNumber.typeMap(Long.TYPE);
        doubleNumber.typeMap(Long.class);
        doubleNumber.typeMap(Long.TYPE);
        
    }

    
}
