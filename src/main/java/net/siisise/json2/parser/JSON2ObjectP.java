package net.siisise.json2.parser;

import java.util.List;
import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFList;
import net.siisise.json2.JSON2Member;
import net.siisise.json2.JSON2Object;

/**
 *
 */
public class JSON2ObjectP extends ABNFList<JSON2Object, JSON2Member> {
    
    public JSON2ObjectP(ABNF def, ABNFReg reg, ABNFReg base) {
        super(def, reg, base, "member");
    }

    @Override
    protected JSON2Object parse(List<JSON2Member> mlist) {
        JSON2Object obj = new JSON2Object();
        if ( mlist != null ) {
            mlist.forEach(mem -> {
                obj.put(mem.str, mem.value);
            });
        }
        return obj;
    }
    
}
