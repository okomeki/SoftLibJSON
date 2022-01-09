/*
 * Copyright 2022 okome.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.siisise.json.stream;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Reader;
import java.io.StringReader;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.stream.JsonParser;
import net.siisise.json.stream.JSONPParserFactory;
import net.siisise.json.stream.JSONPParser;
import net.siisise.json2.JSON2;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

/**
 *
 */
public class JSONPParserFactoryTest {
    
    public JSONPParserFactoryTest() {
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
     * Test of createParser method, of class JSONPParserFactory.
     */
    @Test
    public void testCreateParser_Reader() {
        System.out.println("createParser");
        String json = "123";
        Reader reader = new StringReader(json);
        JSONPParserFactory instance = new JSONPParserFactory();
        JsonParser result = instance.createParser(reader);
        assertNotNull(result);
    }

    /**
     * Test of createParser method, of class JSONPParserFactory.
     */
    @Test
    public void testCreateParser_InputStream() {
        System.out.println("createParser");
        String json = "{\"aa\": 123}";
        InputStream in = new ByteArrayInputStream(json.getBytes(StandardCharsets.UTF_8));
        JSONPParserFactory instance = new JSONPParserFactory();
        JSONPParser result = (JSONPParser) instance.createParser(in);
        assertNotNull(result);

        assertTrue(result.hasNext());
        JsonParser.Event ev = result.next();
        assertEquals(ev, JsonParser.Event.START_OBJECT);

        assertTrue(result.hasNext());
        ev = result.next();
        assertEquals(ev, JsonParser.Event.KEY_NAME);

        assertTrue(result.hasNext());
        ev = result.next();
        assertEquals(ev, JsonParser.Event.VALUE_NUMBER);

        assertTrue(result.hasNext());
        ev = result.next();
        assertEquals(ev, JsonParser.Event.END_OBJECT);
    }

    /**
     * Test of createParser method, of class JSONPParserFactory.
     */
    @Test
    public void testCreateParser_InputStream_Charset() {
        System.out.println("createParser");
        String json = "123";
        InputStream in = new ByteArrayInputStream(json.getBytes(StandardCharsets.UTF_8));
        Charset chrst = StandardCharsets.UTF_8;
        JSONPParserFactory instance = new JSONPParserFactory();
        JsonParser result = instance.createParser(in, chrst);
        assertNotNull(result);
    }

    /**
     * Test of createParser method, of class JSONPParserFactory.
     */
    @Test
    public void testCreateParser_JsonObject() {
        System.out.println("createParser");
        
        JsonObject jobj = (JsonObject) JSON2.parseWrap("{\"aa\": 123}").toJson();
        JSONPParserFactory instance = new JSONPParserFactory();
        JsonParser result = instance.createParser(jobj);
        assertNotNull(result);
        assertTrue(result.hasNext());
        JsonParser.Event ev = result.next();
        assertEquals(ev, JsonParser.Event.START_OBJECT);

        assertTrue(result.hasNext());
        ev = result.next();
        assertEquals(ev, JsonParser.Event.KEY_NAME);

        assertTrue(result.hasNext());
        ev = result.next();
        assertEquals(ev, JsonParser.Event.VALUE_NUMBER);

        assertTrue(result.hasNext());
        ev = result.next();
        assertEquals(ev, JsonParser.Event.END_OBJECT);
    }

    /**
     * Test of createParser method, of class JSONPParserFactory.
     */
    @Test
    public void testCreateParser_JsonArray() {
        System.out.println("createParser");
        JsonArray jsonarray = (JsonArray) JSON2.parseWrap("[\"aa\", 123]").toJson();
        JSONPParserFactory instance = new JSONPParserFactory();
        JsonParser result = instance.createParser(jsonarray);
        assertNotNull(result);
    }

    /**
     * Test of getConfigInUse method, of class JSONPParserFactory.
     */
    @Test
    public void testGetConfigInUse() {
        System.out.println("getConfigInUse");
        JSONPParserFactory instance = new JSONPParserFactory();
        Map expResult = null;
//        Map result = instance.getConfigInUse();
//        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
//        fail("The test case is a prototype.");
    }
    
}
