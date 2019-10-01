package net.siisise.json;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser5234.ABNF5234;
import net.siisise.json.parser.JSONArrayP;
import net.siisise.json.parser.JSONCharP;
import net.siisise.json.parser.JSONMemberP;
import net.siisise.json.parser.JSONNumberP;
import net.siisise.json.parser.JSONObjectP;
import net.siisise.json.parser.JSONStringP;
import net.siisise.json.parser.JSONValueP;
import net.siisise.json.parser.JSONtextParser;

/**
 * RFC 8259 JSON
 *
 * @author okome
 */
public class JSON8259Reg {

    static ABNFReg REG = new ABNFReg(ABNF5234.BASE, ABNF5234.REG);

    public static final ABNF FALSE = REG.rule("false", "%x66.61.6c.73.65");
    public static final ABNF NULL = REG.rule("null", "%x6e.75.6c.6c");
    public static final ABNF TRUE = REG.rule("true", "%x74.72.75.65");
    static final ABNF ws = REG.rule("ws", "*( %x20 / %x09 / %x0A / %x0D )");

    static final ABNF begin_array = REG.rule("begin-array", "ws %x5B ws");
    static final ABNF begin_object = REG.rule("begin-object", "ws %x7B ws");
    static final ABNF end_array = REG.rule("end-array", "ws %x5D ws");
    static final ABNF end_object = REG.rule("end-object", "ws %x7D ws");
    static final ABNF name_separator = REG.rule("name-separator", "ws %x3A ws");
    static final ABNF value_separator = REG.rule("value-separator", "ws %x2C ws");

    public static final ABNF unescaped = REG.rule("unescaped", "%x20-21 / %x23-5B / %x5D-10FFFF");
    public static final ABNF escape = REG.rule("escape", ABNF.bin(0x5c));
    static final ABNF quotation_mark = REG.rule("quotation-mark", ABNF.bin(0x22));
    public static final ABNF CHAR = REG.rule("char", JSONCharP.class, "unescaped / escape ( %x22 / %x5C / %x2F / %x62 / %x66 / %x6E / %x72 / %x74 / %x75 4HEXDIG )");
    public static final ABNF string = REG.rule("string", JSONStringP.class, quotation_mark.pl(CHAR.x(), quotation_mark));
    static final ABNF e = REG.rule("e", "%x65 / %x45");
    static final ABNF minus = REG.rule("minus", ABNF.bin(0x2d));
    static final ABNF plus = REG.rule("plus", ABNF.bin(0x2b));
    public static final ABNF exp = REG.rule("exp", "e [ minus / plus ] 1*DIGIT");
    static final ABNF digit1_9 = REG.rule("digit1-9", ABNF.range(0x31, 0x39));
    static final ABNF decimal_point = REG.rule("decimal-point", ABNF.bin(0x2e));
    public static final ABNF frac = REG.rule("frac", decimal_point.pl(ABNF5234.DIGIT.ix()));
    static final ABNF zero = REG.rule("zero", ABNF.bin(0x30));
    static final ABNF INT = REG.rule("int", zero.or(digit1_9.pl(ABNF5234.DIGIT.x())));
    public static final ABNF number = REG.rule("number", JSONNumberP.class, minus.c().pl(INT, frac.c(), exp.c()));
    public static final ABNF array = REG.rule("array", JSONArrayP.class, "begin-array [ value *( value-separator value ) ] end-array");
    public static final ABNF member = REG.rule("member", JSONMemberP.class, string.pl(name_separator, REG.ref("value")));
    public static final ABNF object = REG.rule("object", JSONObjectP.class, "begin-object [ member *( value-separator member ) ] end-object");
    public static final ABNF value = REG.rule("value", JSONValueP.class, FALSE.or(NULL, TRUE, object, array, number, string));

    public static final ABNF JSONtext = REG.rule("JSON-text", JSONtextParser.class,"ws value ws");

    public static JSONValue parse(String json) {
        return REG.parse("JSON-text",json);
    }
}
