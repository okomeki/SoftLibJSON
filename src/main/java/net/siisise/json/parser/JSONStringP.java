package net.siisise.json.parser;

import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;
import net.siisise.json.JSONString;

/**
 * 文字列.
 * codepoint単位で.
 */
public class JSONStringP extends ABNFList<JSONString, Integer> {

    public JSONStringP(ABNF rule, ABNFReg reg, ABNFReg base) {
        super(rule, reg, base, "char");
    }

    @Override
    public JSONString parse(List<Integer> val) {
        StringBuilder sb = new StringBuilder();
        if (val != null) {
            val.forEach(ch -> {
                sb.appendCodePoint(ch);
            });
        }
        return new JSONString(sb.toString());
    }
}
