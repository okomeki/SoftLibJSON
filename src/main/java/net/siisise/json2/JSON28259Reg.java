package net.siisise.json2;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser5234.ABNF5234;
import net.siisise.json2.parser.JSON2ArrayP;
import net.siisise.json2.parser.JSON2ObjectP;
import net.siisise.json2.parser.JSON2CharP;
import net.siisise.json2.parser.JSON2MemberP;
import net.siisise.json2.parser.JSON2NumberP;
import net.siisise.json2.parser.JSON2StringP;
import net.siisise.json2.parser.JSON2ValueP;
import net.siisise.json2.parser.JSON2textParser;

/**
 * RFC 8259 JSON
 *
 */
public class JSON28259Reg {

    static final ABNFReg REG = new ABNFReg(ABNF5234.BASE, ABNF5234.REG);

    public static final ABNF FALSE = REG.rule("false", "%x66.61.6c.73.65");
    public static final ABNF NULL = REG.rule("null", "%x6e.75.6c.6c");
    public static final ABNF TRUE = REG.rule("true", "%x74.72.75.65");
    static final ABNF ws = REG.rule("ws", "*( %x20 / %x09 / %x0A / %x0D )");

    static final ABNF begin_array = REG.rule("begin-array", ws.pl(ABNF.bin(0x5b), ws)); // [
    static final ABNF begin_object = REG.rule("begin-object", ws.pl(ABNF.bin(0x7b), ws)); // {
    static final ABNF end_array = REG.rule("end-array", ws.pl(ABNF.bin(0x5D), ws)); // ]
    static final ABNF end_object = REG.rule("end-object", ws.pl(ABNF.bin(0x7D), ws)); // }
    static final ABNF name_separator = REG.rule("name-separator", ws.pl(ABNF.bin(0x3A), ws)); // :
    static final ABNF value_separator = REG.rule("value-separator", ws.pl(ABNF.bin(0x2C), ws)); // ,

    public static final ABNF unescaped = REG.rule("unescaped", "%x20-21 / %x23-5B / %x5D-10FFFF");
    public static final ABNF escape = REG.rule("escape", ABNF.bin(0x5c));
    static final ABNF quotation_mark = REG.rule("quotation-mark", ABNF.bin(0x22));
    public static final ABNF CHAR = REG.rule("char", JSON2CharP.class, "unescaped / escape ( %x22 / %x5C / %x2F / %x62 / %x66 / %x6E / %x72 / %x74 / %x75 4HEXDIG )");
    public static final ABNF string = REG.rule("string", JSON2StringP.class, quotation_mark.pl(CHAR.x(), quotation_mark));
    static final ABNF e = REG.rule("e", "%x65 / %x45");
    static final ABNF minus = REG.rule("minus", ABNF.bin(0x2d));
    static final ABNF plus = REG.rule("plus", ABNF.bin(0x2b));
    public static final ABNF exp = REG.rule("exp", "e [ minus / plus ] 1*DIGIT");
    static final ABNF digit1_9 = REG.rule("digit1-9", ABNF.range(0x31, 0x39));
    static final ABNF decimal_point = REG.rule("decimal-point", ABNF.bin(0x2e));
    public static final ABNF frac = REG.rule("frac", decimal_point.pl(ABNF5234.DIGIT.ix()));
    static final ABNF zero = REG.rule("zero", ABNF.bin(0x30));
    static final ABNF INT = REG.rule("int", zero.or(digit1_9.pl(ABNF5234.DIGIT.x())));
    public static final ABNF number = REG.rule("number", JSON2NumberP.class, minus.c().pl(INT, frac.c(), exp.c()));
    public static final ABNF array = REG.rule("array", JSON2ArrayP.class, begin_array.pl(REG.ref("value").pl(value_separator.pl(REG.ref("value")).x()).c(), end_array));
    public static final ABNF member = REG.rule("member", JSON2MemberP.class, string.pl(name_separator, REG.ref("value")));
    public static final ABNF object = REG.rule("object", JSON2ObjectP.class, begin_object.pl(member.pl(value_separator.pl(member).x()).c(), end_object));
    public static final ABNF value = REG.rule("value", JSON2ValueP.class, FALSE.or(NULL, TRUE, object, array, number, string));

    public static final ABNF JSONtext = REG.rule("JSON-text", JSON2textParser.class, ws.pl(value, ws));

    public static Object parse(String json) {
        return REG.parse("JSON-text", json);
    }

    public static Object parse(byte[] json) {
        return REG.parse("JSON-text", json);
    }
}
