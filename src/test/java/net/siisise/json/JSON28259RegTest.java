package net.siisise.json;

import net.siisise.json.JSON2Value;
import net.siisise.json.JSON28259Reg;
import net.siisise.json.JSON2;
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
 */
public class JSON28259RegTest {

    public JSON28259RegTest() {
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
        JSON28259Reg reg = new JSON28259Reg();
        JSON28259Reg.FALSE.is("false");
        List a = new ArrayList();
        Map<String, List> k = new HashMap<>();
        a.add("ふ\"");
        k.put("ni", a);
        String js = JSON2.valueOf(k).toString();
        System.out.println(js);

        System.out.println(js);
        JSON2Value r = JSON2.parseWrap(js);
        System.out.println(r);

        System.out.println(JSON2.valueOf(null));
        System.out.println(JSON2.valueOf(true));
        System.out.println(JSON2.valueOf(false));
        System.out.println(JSON2.valueOf(10));
        System.out.println(JSON2.valueOf("abc"));
        String[] s = new String[]{"a", "b", "c"};
        System.out.println(JSON2.valueOf(s));
        int[] n = new int[]{1, 2, 3};
        System.out.println(JSON2.valueOf(n));
        int[][] d = new int[][]{n, n};
        System.out.println(JSON2.valueOf(d));
        r = JSON2.parseWrap("[\r\n"
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
        System.out.println("伊豆" + JSON28259Reg.REG.href("JSON-text").is("{}"));
//        JSON8259Reg.REG.parse("false","{}");
        JSON28259Reg.REG.parse("value","{}");
    }

}
