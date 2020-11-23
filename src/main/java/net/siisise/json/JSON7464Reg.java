package net.siisise.json;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;

/**
 * JavaScript Object Notation (JSON) Text Sequences.
 * @see https://tools.ietf.org/html/rfc7464
 */
public class JSON7464Reg {
    
    public static String typeName = "application";
    public static String subtypeName = "json-seq";
    
    public static ABNFReg REG = new ABNFReg();

    // 2.1. JSON Text Sequence Parsing
    static ABNF notRS = REG.rule("not-RS","%00-1d / %1f-ff");
    static ABNF possibleJSON = REG.rule("possible-JSON","1*(not-RS)");
    static ABNF RS = REG.rule("RS","%x1E");
    static ABNF inputJSONsequence = REG.rule("input-JSON-sequence","*(1*RS possible-JSON)");

    // 2.2. JSON Text Sequence Encoding
    static ABNF LF = REG.rule("LF","%x0A");
    static ABNF JSONtext = REG.rule("JSON-text",JSON8259Reg.JSONtext); // RFC 7159 から RFC 8259に置換
//    static ABNF RS = REG.rule("RS","%x1E");
    static ABNF JSONsequence = REG.rule("JSON-sequence","*(RS JSON-text LF)");
}
