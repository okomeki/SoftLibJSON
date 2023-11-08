package net.siisise.json.parser;

import java.util.List;
import net.siisise.bnf.BNF;
import net.siisise.bnf.BNFReg;
import net.siisise.bnf.parser.BNFList;

/**
 * 文字列の組み立て
 * char を codepointとして扱う
 */
public class JSONStringP extends BNFList<Object, Integer> {

    public JSONStringP(BNF rule, BNFReg base) {
        super(rule, base, "char");
    }
    
    /**
     * parse済みのcharを繋げるだけ.
     * @param val 文字 ucs-4
     * @return Javaな文字列
     */
    @Override
    protected Object build(List<Integer> val) {
        StringBuilder sb = new StringBuilder();
        if ( val != null ) {
            val.forEach(ch -> {
                sb.appendCodePoint(ch);
            });
        }
//        return sb.toString();
        return ((UnbindABNFReg)base).getFormat().stringFormat(sb.toString());
    }
    
}
