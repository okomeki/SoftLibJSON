package net.siisise.json.pointer;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;

/**
 * JSON Pointer ABNF
 */
public class JSONPointerReg {

    static ABNFReg REG = new ABNFReg();

    static ABNF escaped = REG.rule("escaped", "\"~\" ( \"0\" / \"1\" )");
    static ABNF unescaped = REG.rule("unescaped", "%x00-2E / %x30-7D / %x7F-10FFFF");
    static ABNF referenceToken = REG.rule("reference-token", "*( unescaped / escaped )");
    static ABNF jsonPointer = REG.rule("json-pointer", ABNF.bin('/').pl(referenceToken).x());

    static ABNF arrayIndex = REG.rule("array-index", "%x30 / ( %x31-39 *(%x30-39) )");
}
