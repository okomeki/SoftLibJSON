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
package net.siisise.json;

import net.siisise.json.parser.JSON8259Reg;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;

/**
 * JavaScript Object Notation (JSON) Text Sequences.
 * https://tools.ietf.org/html/rfc7464
 */
public class JSON7464Reg {
    
    public static String typeName = "application";
    public static String subtypeName = "json-seq";
    
    public static final ABNFReg REG = new ABNFReg();

    // 2.1. JSON Text Sequence Parsing
    static final ABNF notRS = REG.rule("not-RS","%00-1d / %1f-ff");
    static final ABNF possibleJSON = REG.rule("possible-JSON","1*(not-RS)");
    static final ABNF RS = REG.rule("RS","%x1E");
    static final ABNF inputJSONsequence = REG.rule("input-JSON-sequence","*(1*RS possible-JSON)");

    // 2.2. JSON Text Sequence Encoding
    static final ABNF LF = REG.rule("LF","%x0A");
    static final ABNF JSONtext = REG.rule("JSON-text",JSON8259Reg.JSONtext); // RFC 7159 から RFC 8259に置換
//    static ABNF RS = REG.rule("RS","%x1E");
    static final ABNF JSONsequence = REG.rule("JSON-sequence","*(RS JSON-text LF)");
}
