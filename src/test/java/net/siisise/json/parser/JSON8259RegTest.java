package net.siisise.json.parser;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import net.siisise.json.JSON;
import net.siisise.json.JSONValue;

/**
 *
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
        String js = JSON.valueOf(k).toJSON();
        System.out.println(js);

        System.out.println(js);
        JSONValue r = JSON.parseWrap(js);
        System.out.println(r);

        System.out.println(JSON.valueOf(null));
        System.out.println(JSON.valueOf(true));
        System.out.println(JSON.valueOf(false));
        System.out.println(JSON.valueOf(10));
        System.out.println(JSON.valueOf("abc"));
        String[] s = new String[]{"a", "b", "c"};
        System.out.println(JSON.valueOf(s));
        int[] n = new int[]{1, 2, 3};
        System.out.println(JSON.valueOf(n));
        int[][] d = new int[][]{n, n};
        System.out.println(JSON.valueOf(d));
        r = JSON.parseWrap("[\r\n"
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
        System.out.println((Object)r.map());
        System.out.println("伊豆" + JSON8259Reg.REG.href("JSON-text").is("{}"));
//        JSON8259Reg.REG.parse("false","{}");
        JSON8259Reg.REG.parse("value","{}");
    }

}
