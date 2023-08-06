/*
 * Copyright 2023 okome.
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
package net.siisise.json.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.block.ReadableBlock;
import net.siisise.io.FrontPacket;

/**
 * Microsoft JSONC.
 * コメントが書けるカンマが増やせる、らしい
 *
 * https://github.com/microsoft/node-jsonc-parser
 */
public class JSONCReg {
    static final ABNFReg REG = new ABNFReg(JSON8259Reg.REG);
    
    /** 
     * * + / 以外
     */
    static final ABNF inmlcomment = REG.rule("inmlcomment", ABNF.range(0x01, 0x29).or(ABNF.range(0x2b,0x10ffff),
                        ABNF.bin('*').pl(ABNF.range(0,0x2e).or(ABNF.range(0x30,0x10ffff)))));
    static final ABNF inslcomment = REG.rule("inslcomment", ABNF.range(0x01,0x09).or(ABNF.bin(0x0b), ABNF.bin(0x0c), ABNF.range(0x0e, 0x10ffff)));

    // コメント /* から */
    static final ABNF mlcomment = REG.rule("wlcomment", ABNF.text("/*").pl(inmlcomment.x(), ABNF.text("*/")));
    // から改行まで
    static final ABNF slcomment = REG.rule("linecomment", ABNF.text("//").pl(inslcomment.x(),ABNF.list("\r\n")));
    static final ABNF ws = REG.rule("ws", slcomment.or(mlcomment, ABNF.list("\u0020\u0009\r\n")).x());

    public static Object parse(String json) {
        return REG.parse("JSON-text", json);
    }

    public static Object parse(byte[] json) {
        return REG.parse("JSON-text", json);
    }

    public static Object parse(FrontPacket json) {
        return REG.parse("JSON-text", json);
    }
    
    public static Object parse(ReadableBlock json) {
        return REG.parse("JSON-text", json);
    }
    
    public static <T> T parse(String name, FrontPacket json) {
        return (T)REG.parse(name, json);
    }
            
    public static <T> T parse(String name, ReadableBlock json) {
        return (T)REG.parse(name, json);
    }
}
