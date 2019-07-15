/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package net.siisise.json;

import java.util.ArrayList;
import java.util.List;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

/**
 *
 * @author okome
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
        JSONValue aajson = JSONValue.valueOf(aa);
        System.out.println(aajson);
        TestA cc = (TestA)aajson.map(TestA.class);
        aajson = JSONValue.valueOf(cc);
        System.out.println(aajson);
//        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
    }

    
}
