package net.siisise.json.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFBaseParser;
import net.siisise.abnf.parser5234.ABNF5234;
import net.siisise.io.FrontPacket;
import net.siisise.lang.CodePoint;

/**
 * 文字解析用
 */
public class JSON2CharP extends ABNFBaseParser<Integer> {

    public JSON2CharP(ABNF rule, ABNFReg base) {
        super(rule);
    }

    static ABNF utf16 = JSON8259Reg.escape.pl(ABNF.bin(0x75), ABNF5234.HEXDIG.x(4, 4));

    @Override
    public Integer parse(FrontPacket pac) {
        FrontPacket p = JSON8259Reg.unescaped.is(pac);
        if (p != null) {
            return CodePoint.utf8(p);
        }
        p = JSON8259Reg.escape.is(pac);
        if (p != null) {
            int es = pac.read();
            switch (es) {
                case 0x22:
                case 0x2f:
                case 0x5c:
                    return es;
                case 0x62:
                    return 0x08;
                case 0x66:
                    return 0x0c;
                case 0x6e:
                    return 0x0a;
                case 0x72:
                    return 0x0d;
                case 0x74: // tab
                    return 0x09;
                case 0x75: // CodePoint-16
                    FrontPacket u = ABNF5234.HEXDIG.x(4, 4).is(pac);
                    if (u != null) {
                        int ch = CodePoint.utf8(u);
                        if (Character.isHighSurrogate((char) ch)) {
                            u = utf16.is(pac);
                            if (u != null) {
                                int ch2 = CodePoint.utf8(u);
                                ch = Character.toCodePoint((char) ch, (char) ch2);
                            }
                        }
                        return ch;
                    } else {
                        pac.backWrite(0x75);
                        pac.backWrite('\\');
                        return null;
                    }
            }
        }
        return null;
    }

}
