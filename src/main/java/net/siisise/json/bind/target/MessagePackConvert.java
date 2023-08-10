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
package net.siisise.json.bind.target;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.charset.StandardCharsets;
import java.time.Clock;
import java.time.Instant;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import net.siisise.bind.Rebind;
import net.siisise.io.PacketA;
import net.siisise.lang.Bin;
import net.siisise.bind.format.BindObject;
import net.siisise.bind.format.ContentBind;

/**
 * MessagePack encoder
 */
public class MessagePackConvert implements ContentBind<byte[]>, BindObject<byte[]> {

    @Override
    public String contentType() {
        return "application/x-msgpack";
    }
    
    byte[] NULL  = new byte[] { (byte)0xc0 };
    byte[] TRUE  = new byte[] { (byte)0xc3 };
    byte[] FALSE = new byte[] { (byte) 0xc2 };
    byte FLOAT = (byte) 0xca;
    byte DOUBLE = (byte) 0xcb;

    @Override
    public byte[] nullFormat() {
        return NULL;
    }

    @Override
    public byte[] booleanFormat(boolean bool) {
        return bool ? TRUE : FALSE;
    }

    @Override
    public byte[] numberFormat(Number num) {

        long lv;
        
        if ( num instanceof BigDecimal ) { // 整数、浮動小数点の分離
            if (((BigDecimal)num).scale() == 0 ) {
                num = ((BigDecimal) num).toBigInteger();
            } else {
                num = num.doubleValue();
            }
        }
        
        if ( num instanceof BigInteger ) {
            if ( ((BigInteger) num).compareTo(BigInteger.valueOf(Long.MAX_VALUE)) <= 0 &&
                 ((BigInteger)num).compareTo(BigInteger.valueOf(Long.MIN_VALUE)) >= 0 ) {
                lv = num.longValue();
            } else {
                throw new UnsupportedOperationException("まだない");
            }
        } else if ( num instanceof Byte || num instanceof Short || num instanceof Integer || num instanceof Long ) {
            lv  = num.longValue();
        } else if ( num instanceof Float ) {
            byte[] f = new byte[5];
            f[0] = FLOAT;
            int fv = Float.floatToIntBits(num.floatValue());
            Bin.toByte(fv,f,1);
            return f;
        } else if ( num instanceof Double ) {
            byte[] f = new byte[9];
            f[0] = DOUBLE;
            long dv = Double.doubleToLongBits(num.doubleValue());
            //long dv = Double.doubleToRawLongBits(num.doubleValue());
            Bin.toByte(dv,f,1);
            return f;
        } else {
                throw new UnsupportedOperationException("まだない");
        }
        if ( lv >= -32 && lv <= 0x7f ) { // 00 ff
            return new byte[] { (byte) lv};
        } else if ( lv >= -128 ) {
            return new byte[] {(byte)0xd0, (byte)lv};
        } else if ( lv <= 0xff ) {
            byte[] d = new byte[2];
            d[0] = (byte)0xcc;
            d[1] = (byte)lv;
            return d;
        } else if ( lv >= Short.MIN_VALUE && lv <= Short.MAX_VALUE ) {
            byte[] d = new byte[3];
            d[0] = (byte)0xd1;
            d[1] = (byte)(lv >> 8);
            d[2] = (byte)lv;
            return d;
        } else if ( lv <= 0xffff ) {
            byte[] d = new byte[3];
            d[0] = (byte)0xcd;
            d[1] = (byte)(lv >> 8);
            d[2] = (byte)lv;
            return d;
        } else if ( lv >= Integer.MIN_VALUE && lv <= Integer.MAX_VALUE ) {
            byte[] d = new byte[5];
            d[0] = (byte)0xd2;
            Bin.toByte((int)lv,d,1);
            return d;
        } else if ( lv <= 0xffffffffl ) {
            byte[] d = new byte[5];
            d[0] = (byte)0xce;
            Bin.toByte((int)lv,d,1);
            return d;
        } else { // long
            byte[] d = new byte[9];
            d[0] = (byte)0xd2;
            Bin.toByte(lv,d,1);
            return d;
        }
    }

    @Override
    public byte[] stringFormat(String str) {
        byte[] v = str.getBytes(StandardCharsets.UTF_8);
        byte[] r;
        if ( v.length < 32) {
            r = new byte[v.length+1];
            r[0] = (byte)(0xa0 + v.length);
            System.arraycopy(v, 0, r, 1, v.length);
            return r;
        } else if ( v.length < 0x100 ) {
            r = new byte[v.length+2];
            r[0] = (byte)0xd9;
            r[1] = (byte)v.length;
            System.arraycopy(v, 0, r, 2, v.length);
            return r;
        } else if ( v.length < 0x10000 ) {
            r = new byte[v.length+3];
            r[0] = (byte)0xda;
            r[1] = (byte)(v.length >> 8);
            r[2] = (byte)v.length;
            System.arraycopy(v, 0, r, 3, v.length);
            return r;
        } else { // length は Integer.MAX_LENGTH ぐらいまでなので
            PacketA pac = new PacketA();
            pac.write(0xdb);
            pac.dwrite(Bin.toByte(v.length));
            pac.dwrite(v);
            return pac.toByteArray();
        }
    }

    @Override
    public byte[] collectionFormat(Collection list) {
        int size = list.size();
        PacketA pac = new PacketA();
        if ( size < 16 ) {
            pac.write(0x90 + size);
        } else if ( size < 0x10000 ) {
            pac.write(0xdc);
            pac.write(size >> 8 & 0xff);
            pac.write(size & 0xff);
        } else {
            pac.write(0xdd);
            pac.dwrite(Bin.toByte(size));
        }
        for ( Object val : list ) {
            pac.dwrite((byte[])Rebind.valueOf(val, this));
        }
        return pac.toByteArray();
    }

    @Override
    public byte[] byteArrayFormat(byte[] bin) {
        PacketA pac = new PacketA();
        if ( bin.length < 0x100 ) {
            pac.write(0xc4);
            pac.write(bin.length);
        } else if ( bin.length < 0x10000 ) {
            pac.write(0xc5);
            pac.write(bin.length >> 8);
            pac.write(bin.length & 0xff);
        } else {
            pac.write(0xc6);
            pac.dwrite(Bin.toByte(bin.length));
        }
        pac.write(bin);
        return pac.toByteArray();
    }

    @Override
    public byte[] mapFormat(Map map) {
        int size = map.size();
        PacketA pac = new PacketA();
        if ( size < 16 ) {
            pac.write(0x80 + size);
        } else if ( size < 0x10000 ) {
            pac.write(0xde);
            pac.write(size >> 8 & 0xff);
            pac.write(size & 0xff);
        } else {
            pac.write(0xdf);
            pac.write(Bin.toByte(size));
        }
        for ( Map.Entry<?,?> entry : ((Map<String,?>)map).entrySet() ) {
            pac.dwrite((byte[])stringFormat((String)entry.getKey()));
            pac.dwrite((byte[])Rebind.valueOf(entry.getValue(),this));
        }
        return pac.toByteArray();
    }

    @Override
    public byte[] objectFormat(Object obj) {
        if ( obj instanceof Date) { // 時間系
            Instant inst = Instant.ofEpochMilli(((Date) obj).getTime());  // GMTとUTCの差があるかもしれない
            return time(inst);
        } else if ( obj instanceof Clock ) {
            Instant inst = Instant.now((Clock) obj);
            return time(inst);
        }
        return mapFormat(Rebind.valueOf(obj, Map.class));
    }
    
    private byte[] time(Instant inst) {
        PacketA pac = new PacketA();
        long sec = inst.getEpochSecond();
        int nano = inst.getNano();
        if ( inst.getNano() == 0 && sec <= Integer.MAX_VALUE) {
            pac.write(0xd6);
            pac.write(0xff);
            pac.dwrite(Bin.toByte((int)sec));
        } else if ( (nano & 0x03) == 0 && sec <= 0x3ffffffffl ) {
            sec |= ((long)nano) << 34;
            pac.write(0xd7);
            pac.write(0xff);
            pac.dwrite(Bin.toByte(sec));
        } else {
            // 0xd6 seconds in 32-bit unsigned int
            pac.write(0xd7);
            pac.write(12);
            pac.write(0xff);
            pac.dwrite(Bin.toByte(inst.getNano()));
            pac.dwrite(Bin.toByte(inst.getEpochSecond()));
        }
        return pac.toByteArray();
    }
    
}
