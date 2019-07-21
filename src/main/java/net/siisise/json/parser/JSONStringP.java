package net.siisise.json.parser;

import java.util.List;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;
import net.siisise.json.JSON8259Reg;
import net.siisise.json.JSONString;

/**
 *
 * @author okome
 */
public class JSONStringP extends ABNFList<JSONString, Integer> {

    public JSONStringP(ABNFReg reg) {
        super(JSON8259Reg.string, reg, JSONCharP.class);
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
