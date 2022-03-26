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
package net.siisise.msgpack;

import java.math.BigInteger;
import java.util.Map;
import net.siisise.io.FrontPacket;
import net.siisise.io.Packet;
import net.siisise.io.PacketA;
import net.siisise.json.JSON;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONObject;
import net.siisise.json.JSONValue;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

/**
 *
 */
public class MessagePackTest {
    
    public MessagePackTest() {
    }
    
    static byte[] BIN = {(byte)0x82,(byte)0xa7,0x63,0x6f,0x6d,0x70,0x61,0x63,0x74,(byte)0xc3,(byte)0xa6,0x73,0x63,0x68,0x65,0x6d,0x61,0x00};
    static String STR = "{\"compact\":true,\"schema\":0}";

    /**
     * Test of parse method, of class MessagePack.
     */
    @Test
    public void testParse() {
        System.out.println("parse");
        Packet pac = new PacketA();
        pac.write(BIN);
        MessagePack instance = new MessagePack();
        Object expResult = STR;
        Object result = instance.parse(pac);
        assertEquals(expResult, result.toString());
    }

    /**
     * Test of array method, of class MessagePack.
     */
    @Test
    public void testArray() {
        System.out.println("array");
        PacketA pac = new PacketA();
        pac.write(1);
        pac.write(0);
        pac.write(new byte[256]); // 0の列
        int l = 2;
        MessagePack instance = new MessagePack();
        JSONArray expResult = new JSONArray();
        for ( int i = 0; i < 256; i++ ) {
            expResult.add(Integer.valueOf(0));
        }
        JSONArray result = instance.array(pac, l);
        assertEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
    }

    /**
     * Test of build method, of class MessagePack.
     */
    @Test
    public void testBuild() {
        System.out.println("build");
        JSONValue obj = JSON.parseWrap(STR);
        System.out.println(obj.toJSON());
        //MessagePack instance = new MessagePack();
        byte[] expResult = BIN;
        byte[] result = MessagePack.build(obj);
        assertArrayEquals(expResult, result);
        // TODO review the generated test code and remove the default call to fail.
        //fail("The test case is a prototype.");
    }
    
}
