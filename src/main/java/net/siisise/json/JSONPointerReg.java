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
package net.siisise.json;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;

/**
 * JSON Pointer ABNF
 */
public class JSONPointerReg {

    static ABNFReg REG = new ABNFReg();

    static ABNF escaped = REG.rule("escaped", "\"~\" ( \"0\" / \"1\" )");
    static ABNF unescaped = REG.rule("unescaped", "%x00-2E / %x30-7D / %x7F-10FFFF");
    static ABNF referenceToken = REG.rule("reference-token", unescaped.or(escaped).x());
    static ABNF jsonPointer = REG.rule("json-pointer", ABNF.bin('/').pl(referenceToken).x());

    static ABNF arrayIndex = REG.rule("array-index", "%x30 / ( %x31-39 *(%x30-39) )");
}
