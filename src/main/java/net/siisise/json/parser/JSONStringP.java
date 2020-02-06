package net.siisise.json.parser;

import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;
import net.siisise.json.JSONString;

/**
 *
 */
public class JSONStringP extends ABNFList<JSONString, Integer> {

    public JSONStringP(ABNF def, ABNFReg reg, ABNFReg base) {
        super(def, reg, base, "char");
    }

    @Override
    public JSONString parse(List<Integer> val) {
        StringBuilder sb = new StringBuilder();
        if (val != null) {
            for (Integer ch : val) {
                sb.appendCodePoint(ch);
            }
        }
        return new JSONString(sb.toString());
    }
}
