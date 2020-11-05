package net.siisise.json2.parser;

import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;

/**
 *
 */
public class JSON2StringP extends ABNFList<String, Integer> {

    public JSON2StringP(ABNF def, ABNFReg reg, ABNFReg base) {
        super(def, reg, base, "char");
    }
    
    @Override
    protected String parse(List<Integer> val) {
        StringBuilder sb = new StringBuilder();
        if ( val != null ) {
            val.forEach(ch -> {
                sb.appendCodePoint(ch);
            });
        }
        return sb.toString();
    }
    
}
