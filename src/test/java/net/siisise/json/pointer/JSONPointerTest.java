package net.siisise.json.pointer;

import net.siisise.json.JSONPointer;
import net.siisise.json.JSON2;
import net.siisise.json.JSON2Collection;
import net.siisise.json.JSON2Value;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

/**
 *
 */
public class JSONPointerTest {
    
    public JSONPointerTest() {
    }
    
    @BeforeAll
    public static void setUpClass() {
    }
    
    @AfterAll
    public static void tearDownClass() {
    }

    /**
     * Test of sub method, of class JSONPointer.
     */
/*    @Test
    public void testSub() {
        System.out.println("sub");
        JSONPointer instance = null;
        JSONPointer expResult = null;
        JSONPointer result = instance.sub();
        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
    }
*/
    /**
     * Test of toString method, of class JSONPointer.
     */
/*    @Test
    public void testToString() {
        System.out.println("toString");
        JSONPointer instance = new JSONPointer("0/2/2");
        String expResult = "";
        String result = instance.toString();
        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
    }
*/
    /**
     * Test of toDecodeString method, of class JSONPointer.
     */
/*    @Test
    public void testToDecodeString() {
        System.out.println("toDecodeString");
        JSONPointer instance = null;
        String[] expResult = null;
        String[] result = instance.toDecodeString();
        assertArrayEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
    }
*/  
    @Test
    public void testOther() {
        int[] a = {1, 2, 3};
        int[][] b = {a};
        JSON2Collection val = (JSON2Collection) JSON2.valueOf(b);
        JSON2Value t = new JSONPointer("/0/2").get(val);
        System.out.println(t);
    }
    
}
