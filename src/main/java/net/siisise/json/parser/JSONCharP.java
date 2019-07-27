package net.siisise.json.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.AbstractABNF;
import net.siisise.abnf.parser.ABNFBaseParser;
import net.siisise.abnf.parser5234.ABNF5234;
import net.siisise.io.Packet;
import net.siisise.json.JSON8259Reg;
import net.siisise.json.JSONValue;

/**
 * JSONChar の代わりにIntegerを使用する
 *
 * @author okome
 */
public class JSONCharP extends ABNFBaseParser<Integer, JSONValue> {

    public JSONCharP(ABNF def, ABNFReg reg, ABNFReg base) {
        super(def, reg);
    }

    static ABNF utf16 = JSON8259Reg.escape.pl(ABNF.bin(0x75), ABNF5234.HEXDIG.x(4, 4));

    @Override
    public Integer parse(Packet pac) {
        Packet p = JSON8259Reg.unescaped.is(pac);
        if (p != null) {
            return AbstractABNF.utf8(p);
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
                case 0x75: // UTF-16
                    Packet u = ABNF5234.HEXDIG.x(4, 4).is(pac);
                    if (u != null) {
                        int ch = AbstractABNF.utf8(u);
                        if (Character.isHighSurrogate((char) ch)) {
                            u = utf16.is(pac);
                            if (u != null) {
                                int ch2 = AbstractABNF.utf8(u);
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
