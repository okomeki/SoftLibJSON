package net.siisise.json.parser;

import java.util.List;
import net.siisise.bnf.BNF;
import net.siisise.bnf.BNFReg;
import net.siisise.bnf.parser.BNFList;

/**
 * 文字列の組み立て
 * char を codepointとして扱う
 */
public class JSONStringP extends BNFList<String, Integer> {

    public JSONStringP(BNF rule, BNFReg base) {
        super(rule, base, "char");
    }
    
    @Override
    protected String build(List<Integer> val) {
        StringBuilder sb = new StringBuilder();
        if ( val != null ) {
            val.forEach(ch -> {
                sb.appendCodePoint(ch);
            });
        }
        return sb.toString();
    }
    
}
