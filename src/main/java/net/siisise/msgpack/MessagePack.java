/*
 * Copyright 2022 Siisise Net.
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
import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Date;
import java.util.Map;
import net.siisise.io.FrontPacket;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONObject;
import net.siisise.json.bind.OMAP;

/**
 * MessagePack の仮Parser
 * #msgpack
 * 
 * 配列 は配列のままかもしれないし、JSONArrayかもしれない
 * バイナリ列はbyte[]
 * MapはJSONObject型のMap
 * extはとりあえずJSONArray
 * 数値はNumberを継承した何か BigIntegerになっていることが多いかもしれない
 * boolean, null はJavaの型 Boolean と null
 * toJSON()系でとりあえずJSONも吐き出せるかもしれない
 * 
 */
public class MessagePack {
    
    public Object parse(FrontPacket pac) {
        int cmd = pac.read();
        
        switch ( cmd ) {
            case 0xc0:
                return null;
            case 0xc1:
                throw new java.lang.UnsupportedOperationException();
            case 0xc2:
                return false; // JSONBoolean.FALSE;
            case 0xc3:
                return true; // JSONBoolean.TRUE;
            case 0xc4:
                cmd = pac.read();
                return bin(cmd, pac);
            case 0xc5:
                cmd = (int) readLong(pac,2);
                return bin(cmd, pac);
            case 0xc6:
                cmd = (int) readLong(pac,4);
                if ( cmd < 0 ) {
                    throw new java.lang.UnsupportedOperationException();
                }
                return bin(cmd, pac); // 1.5GBぐらいまで限定 (BASE64URL化する)
            case 0xc7:
                cmd = pac.read();
                return ext(cmd, pac);
            case 0xc8:
                cmd = (int) readLong(pac,2);
                return ext(cmd, pac);
            case 0xc9:
                cmd = (int) readLong(pac,4);
                if ( cmd < 0 ) {
                    throw new java.lang.UnsupportedOperationException();
                }
                return ext(cmd, pac); // 1.5GBぐらいまで限定 (BASE64URL化する)
            case 0xca:
                return float32(pac);
            case 0xcb:
                return float64(pac);
            case 0xcc:
                return parseUnsignedNumber(pac,1);
            case 0xcd:
                return parseUnsignedNumber(pac,2);
            case 0xce:
                return parseUnsignedNumber(pac,4);
            case 0xcf:
                return parseUnsignedNumber(pac,8);
            case 0xd0:
                return parseNumber(pac,1);
            case 0xd1:
                return parseNumber(pac,2);
            case 0xd2:
                return parseNumber(pac,4);
            case 0xd3:
                return parseNumber(pac,8);
            case 0xd4:
                return ext(1,pac);
            case 0xd5:
                return ext(2,pac);
            case 0xd6:
                return ext(4,pac);
            case 0xd7:
                return ext(8,pac);
            case 0xd8:
                return ext(16,pac);
            case 0xd9:
                return str(pac,1);
            case 0xda:
                return str(pac,2);
            case 0xdb:
                return str(pac,4);
            case 0xdc:
                return array(pac,2);
            case 0xdd:
                return array(pac,4);
            case 0xde:
                return map(pac,2);
            case 0xdf:
                return map(pac,4);
                
            default:
                if ( cmd <= 0x7f ) { // Number
                    return cmd;
                } else if ( cmd <= 0x8f ) { // Map
                    return parseMap(cmd & 0xf, pac);
                } else if ( cmd <= 0x9f ) { // array
                    return parseArray(cmd & 0x0f, pac);
                } else if ( cmd <= 0xbf ) {
                    return parseStr(cmd & 0x1f, pac);
                } else if ( cmd >= 0xe0 ) {
                    return (cmd | 0xffffff00);
                }
                throw new java.lang.UnsupportedOperationException();
        }
    }
    
    JSONObject parseMap(int count, FrontPacket pac) {
        JSONObject obj = new JSONObject();
        
        for ( int i = 0; i < count; i++) {
            String name = (String) parse(pac);
            Object value = parse(pac);
            obj.put(name, value);
        }
        return obj;
    }

    JSONArray parseArray(int count, FrontPacket pac) {
        JSONArray array = new JSONArray();
        
        for (int i = 0; i < count; i++ ) {
            array.add(parse(pac));
        }
        return array;
    }
    
    String parseStr(int count, FrontPacket pac) {
        byte[] bin = new byte[count];
        pac.read(bin);
        return new String(bin,StandardCharsets.UTF_8);
    }
    
    /**
     * 可能な範囲でBASE64 URLエンコードする
     * @param count
     * @param pac
     * @return 
     */
    byte[] bin(int count, FrontPacket pac) {
        byte[] data = new byte[count];
        pac.read(data);
        return data;
    }
    
    /**
     * 
     * @param len 長さ
     * @param pac
     * @return 
     */
    Object ext(int len, FrontPacket pac) {
        JSONArray ex = new JSONArray();
        int type = pac.read();
        if ( type == -1 ) {
            if ( len == 4 ) {
                long sec = readLong(pac,4);
                return Date.from(Instant.ofEpochSecond(sec));
            } else if (len == 8 || len == 12) {
                long nano, sec;
                if ( len == 8 ) {
                    nano = readLong(pac,4);
                    sec = readLong(pac,4);
                    sec |= (nano & 0x3) << 32;
                    nano >>= 2;
                } else {
                    nano = readLong(pac,4);
                    sec = readLong(pac,8);
                }
                return Date.from(Instant.ofEpochSecond(sec, nano));
            }
        }
        ex.add(type);
        byte[] data = new byte[len];
        pac.read(data);
        ex.add(data);
        return ex;
    }
    
    Float float32(FrontPacket pac) {
        byte[] data = new byte[4];
        pac.read(data);
        int f = 0;
        for ( int i = 0; i < 4; i++ ) {
            f <<= 8;
            f |= data[i] & 0xff;
        }
        return Float.intBitsToFloat(f);
    }

    Double float64(FrontPacket pac) {
        byte[] data = new byte[8];
        pac.read(data);
        long d = 0;
        for ( int i = 0; i < 8; i++ ) {
            d <<= 8;
            d |= data[i] & 0xff;
        }
        return Double.longBitsToDouble(d);
    }
    
    private BigInteger parseUnsignedNumber(FrontPacket pac, int len) {
        byte[] n = new byte[len+1];
        pac.read(n,1,len);
        return new BigInteger(n);
    }

    private BigInteger parseNumber(FrontPacket pac, int len) {
        byte[] d = new byte[len];
        pac.read(d);
        return new BigInteger(d);
    }

    private long readLong(FrontPacket pac, int len) {
        long l = 0;
        for ( int i = 0; i < len; i++ ) {
            l <<= 8;
            l |= pac.read();
        }
        return l;
    }
    
    String str(FrontPacket pac, int l) {
        byte[] n = new byte[l+1];
        pac.read(n,1,l);
        long ln = new BigInteger(n).longValue();
        byte[] data = new byte[(int)ln];
        pac.read(data);
        return new String(data,StandardCharsets.UTF_8);
    }
    
    JSONArray array(FrontPacket pac, int l ) {
        byte[] n = new byte[l+1];
        pac.read(n,1,l);
        long len = new BigInteger(n).longValue();
        return parseArray((int) len,pac);
    }
    
    Map map(FrontPacket pac, int l ) {
        byte[] n = new byte[l+1];
        pac.read(n,1,l);
        long len = new BigInteger(n).longValue();
        return parseMap((int) len,pac);
    }
    
    public static byte[] build(Object obj) {
        return OMAP.valueOf(obj, MessagePack.class);
    }
    
}
