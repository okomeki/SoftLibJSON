package net.siisise.json.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser5234.ABNF5234;
import net.siisise.bind.format.TypeFormat;
import net.siisise.block.ReadableBlock;
import net.siisise.io.FrontPacket;
import net.siisise.json.bind.target.JSONInConvert;

/**
 * RFC 8259 The JavaScript Object Notation (JSON) Data Interchange Format.
 * ABNF
 *
 */
public class JSON8259Reg {
    
    static final TypeFormat<Object> format = new JSONInConvert();
    public static final ABNFReg REG = new UnbindABNFReg(ABNF5234.BASE, format);

//    static final ABNFReg REG = new ABNFReg(ABNF5234.BASE);

    public static final ABNF FALSE = REG.rule("false", "%x66.61.6c.73.65");
    public static final ABNF NULL = REG.rule("null", "%x6e.75.6c.6c");
    public static final ABNF TRUE = REG.rule("true", "%x74.72.75.65");
    // JSONCで拡張するので直接参照しない方がいいかも
    static final ABNF ws = REG.rule("ws", ABNF.list("\u0020\u0009\r\n").x());

    public static final ABNF begin_array = REG.rule("begin-array", REG.ref("ws").pl(ABNF.bin(0x5b), REG.ref("ws"))); // [
    public static final ABNF begin_object = REG.rule("begin-object", REG.ref("ws").pl(ABNF.bin(0x7b), REG.ref("ws"))); // {
    public static final ABNF end_array = REG.rule("end-array", REG.ref("ws").pl(ABNF.bin(0x5D), REG.ref("ws"))); // ]
    public static final ABNF end_object = REG.rule("end-object", REG.ref("ws").pl(ABNF.bin(0x7D), REG.ref("ws"))); // }
    public static final ABNF name_separator = REG.rule("name-separator", REG.ref("ws").pl(ABNF.bin(0x3A), REG.ref("ws"))); // :
    public static final ABNF value_separator = REG.rule("value-separator", REG.ref("ws").pl(ABNF.bin(0x2C), REG.ref("ws"))); // ,

    public static final ABNF unescaped = REG.rule("unescaped", ABNF.range(0x20,0x21).or1(ABNF.range(0x23,0x5b), ABNF.range(0x5d,0x10ffff)));
    public static final ABNF escape = REG.rule("escape", ABNF.bin(0x5c));
    static final ABNF quotation_mark = REG.rule("quotation-mark", ABNF.bin(0x22));
    public static final ABNF CHAR = REG.rule("char", JSONCharP.class, unescaped.or1(escape.pl(ABNF.list("\"\\/bfnrt").or1(ABNF.bin(0x75).pl(ABNF5234.HEXDIG.x(4,4))))));
    public static final ABNF string = REG.rule("string", JSONStringP.class, quotation_mark.pl(CHAR.x(), quotation_mark));
    static final ABNF e = REG.rule("e", ABNF.bin(0x65).or1(ABNF.bin(0x45)));
    static final ABNF minus = REG.rule("minus", ABNF.bin(0x2d));
    static final ABNF plus = REG.rule("plus", ABNF.bin(0x2b));
    public static final ABNF exp = REG.rule("exp", "e [ minus / plus ] 1*DIGIT");
    static final ABNF digit1_9 = REG.rule("digit1-9", ABNF.range(0x31, 0x39));
    static final ABNF decimal_point = REG.rule("decimal-point", ABNF.bin(0x2e));
    public static final ABNF frac = REG.rule("frac", decimal_point.pl(ABNF5234.DIGIT.ix()));
    static final ABNF zero = REG.rule("zero", ABNF.bin(0x30));
    static final ABNF INT = REG.rule("int", zero.or(digit1_9.pl(ABNF5234.DIGIT.x())));
    public static final ABNF number = REG.rule("number", JSONNumberP.class, minus.c().pl(INT, frac.c(), exp.c()));
    public static final ABNF array = REG.rule("array", JSONArrayP.class, begin_array.pl(REG.ref("value").pl(value_separator.pl(REG.ref("value")).x()).c(), end_array));
    public static final ABNF member = REG.rule("member", JSONMemberP.class, string.pl(name_separator, REG.ref("value")));
    public static final ABNF object = REG.rule("object", JSONObjectP.class, begin_object.pl(member.pl(value_separator.pl(member).x()).c(), end_object));
    public static final ABNF value = REG.rule("value", JSONValueP.class, FALSE.or1(NULL, TRUE, object, array, number, string));

    public static final ABNF JSONtext = REG.rule("JSON-text", JSONtextParser.class, REG.ref("ws").pl(value, REG.ref("ws")));

    public static Object parse(String json) {
        return REG.parse("JSON-text", json);
    }

    public static <T> T parse(String json, TypeFormat<T> format) {
        return format(format).parse("JSON-text", json);
    }

    public static Object parse(byte[] json) {
        return REG.parse("JSON-text", json);
    }

    public static <T> T parse(byte[] json, TypeFormat<T> format) {
        return format(format).parse("JSON-text", json);
    }

    public static Object parse(FrontPacket json) {
        return REG.parse("JSON-text", json);
    }
    
    public static <T> T parse(FrontPacket json, TypeFormat<T> format) {
        return (T) format(format).parse("JSON-text", json);
    }

    public static Object parse(ReadableBlock json) {
        return REG.parse("JSON-text", json);
    }

    public static <T> T parse(ReadableBlock json, TypeFormat<T> format) {
        return format(format).parse("JSON-text", json);
    }
    
    public static <T> T parse(String name, FrontPacket json) {
        return (T)REG.parse(name, json);
    }
            
    public static <T> T parse(String name, ReadableBlock json) {
        return (T)REG.parse(name, json);
    }
    
    public static UnbindABNFReg format(TypeFormat format) {
        return new UnbindABNFReg(REG, format);
    }
}
