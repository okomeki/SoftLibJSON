package net.siisise.json;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

/**
 *
 * @author okome
 */
public class JSON8259RegTest {

    public JSON8259RegTest() {
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

    @Test
    public void testValues() {
        JSON8259Reg reg = new JSON8259Reg();
        JSON8259Reg.FALSE.is("false");
        List a = new ArrayList();
        Map<String, List> k = new HashMap<>();
        a.add("ふ\"");
        k.put("ni", a);
        String js = JSONValue.valueOf(k).toString();
        System.out.println(js);

        System.out.println(js);
        JSONValue r = JSON8259Reg.parse(js);
        System.out.println(r);

        System.out.println(JSONValue.valueOf(null));
        System.out.println(JSONValue.valueOf(true));
        System.out.println(JSONValue.valueOf(false));
        System.out.println(JSONValue.valueOf(10));
        System.out.println(JSONValue.valueOf("abc"));
        String[] s = new String[]{"a", "b", "c"};
        System.out.println(JSONValue.valueOf(s));
        int[] n = new int[]{1, 2, 3};
        System.out.println(JSONValue.valueOf(n));
        int[][] d = new int[][]{n, n};
        System.out.println(JSONValue.valueOf(d));
        r = JSON.parse("[\r\n"
                + "  {\r\n"
                + "    \"name\": \"Taro\",\r\n"
                + "    \"age\": 30,\r\n"
                + "    \"languages\": [\"Japanese\", \"English\"],\r\n"
                + "    \"active\": true\r\n"
                + "  },\r\n"
                + "  {\r\n"
                + "    \"name\": \"Aiko\",\r\n"
                + "    \"age\": 33,\r\n"
                + "    \"languages\": [\"Japanese\"],\r\n"
                + "    \"active\": false\r\n"
                + "  },\r\n"
                + "  {\r\n"
                + "    \"name\": \"Hanako\",\r\n"
                + "    \"age\": 29,\r\n"
                + "    \"languages\": [\"English\", \"French\"],\r\n"
                + "    \"active\": true\r\n"
                + "  }\r\n"
                + "]");
        System.out.println(r);
        System.out.println(r.map());
        System.out.println("伊豆" + JSON8259Reg.REG.href("JSON-text").is("{}"));
//        JSON8259Reg.REG.parse("false","{}");
        JSON8259Reg.REG.parse("value","{}");
    }

}
