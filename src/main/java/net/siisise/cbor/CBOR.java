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
package net.siisise.cbor;

import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import net.siisise.io.FrontPacket;
import net.siisise.io.PacketA;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONObject;
import net.siisise.json.bind.OMAP;

/**
 * RFC 8949 CBOR.
 * MessagePack に似ているもの
 */
public class CBOR {
    
    public static Object parse(byte[] src) {
        return parse(new PacketA(src));
    }
    
    public static Object parse(FrontPacket pac) {
        int code = pac.read();
        long len = 0;
        switch ( code & 0x1f ) {
            case 24:
                len = pac.read();
                break;
            case 25:
                len = pac.read();
                len <<= 8;
                len |= pac.read();
                break;
            case 26:
                for ( int i = 0; i < 4; i++ ) {
                    len <<= 8;
                    len |= pac.read();
                }
                break;
            case 27:
                for ( int i = 0; i < 8; i++ ) {
                    len <<= 8;
                    len |= pac.read();
                }
                break;
            case 28:
            case 29:
            case 30:
                throw new UnsupportedOperationException("予約コード");
            case 31: // 引数なし 仮に-1
                len = -1;
                break;
            default:
                len = code & 0x1f;
                break;
                
        }
        if ( len == -1 ) {
            switch ( code >> 5 ) {
                case 7:
                    throw new UnsupportedOperationException("breakコード");
                    
            }
            throw new UnsupportedOperationException("予約コード");
        }
        
        switch ( code >> 5 ) {
            case 0: // 符号無し整数
                return len;
            case 1: // 不
                return -len-1;
            case 2: // bin 大きいものを扱うときはPacket形式に変えるかも
                if ( len == -1 ) { // nest
                    PacketA ret = new PacketA();
                    Object v = parse(pac);
                    while ( v != CBOR.class ) {
                        ret.dwrite((byte[])v);
                        v = parse(pac);
                    }
                    return ret.toByteArray();
                }
                byte[] bin = new byte[(int)len];
                pac.read(bin);
                return bin;
            case 3: // UTF-8 String Javaで扱えそうな範囲内で
                PacketA ret = new PacketA();
                if ( len == -1) {
                    Object v = parse(pac);
                    while ( v != CBOR.class ) {
                        ret.write((byte[])v);
                        v = parse(pac);
                    }
                    return new String(ret.toByteArray(), StandardCharsets.UTF_8);
                } else {
                    byte[] str = new byte[(int)len];
                    pac.read(str);
                    return new String(str,StandardCharsets.UTF_8);
                }
            case 4: // 配列/List
                JSONArray list = new JSONArray();
                if ( len == -1 ) {
                    Object v = parse(pac);
                    while ( v != CBOR.class ) {
                        list.add(v);
                        v = parse(pac);
                    }
                } else {
                    for ( int i = 0; i < len; i++ ) {
                        list.add(parse(pac));
                    }
                }
                return list;
            case 5:
                JSONObject obj = new JSONObject();
                if ( len == -1 ) {
                    Object k = parse(pac);
                    while ( k != CBOR.class ) {
                        Object val = parse(pac);
                        obj.put((String)k, val);
                        k = parse(pac);
                    }
                } else {
                    for ( int i = 0; i < len; i++ ) {
                        String key  = (String) parse(pac);
                        Object val = parse(pac);
                        obj.put(key, val);
                    }
                }
                return obj;
            case 6: // tag Section 3.4.
                throw new UnsupportedOperationException();
            case 7: // float / simple / Section 3.3.
                switch ( code & 0x1f) {
                    case 24:
                        switch ( (int)len ) {
                            case 20:
                                return false;
                            case 21:
                                return true;
                            case 22:
                                return null;
                            case 23:
                                return null; // undefined
                            default:
                                // 0..19 unassigned
                                // 24..31 reserved
                                // 32..255 unassigned
                                throw new UnsupportedOperationException("Reserved Javaに16bitがない");
                        }
                    case 25: // IEEE 754 16bit float
                        throw new UnsupportedOperationException("Reserved Javaに16bitがない");
                    case 26: // IEEE 754 Single-Percision Float (32 bits follow) 
                        return Float.intBitsToFloat((int)len);
                    case 27: // IEEE 754 Single-Percision Float (64 bits follow)
                        return Double.longBitsToDouble(len);
                    case 28:
                    case 29:
                    case 30:
                        throw new UnsupportedOperationException("Reserved");
                    case 31:
                        return CBOR.class; // break の代わり
                    default: // 0 to 255
                    throw new UnsupportedOperationException();
                }
//                throw new UnsupportedOperationException();
            default:
                throw new UnsupportedOperationException();
        }
    }
    
    public static byte[] build(Object obj) {
        return OMAP.valueOf(obj, CBOR.class);
    }
}
