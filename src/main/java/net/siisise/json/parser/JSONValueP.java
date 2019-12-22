package net.siisise.json.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFSelect;
import net.siisise.io.Packet;
import net.siisise.json.JSON8259Reg;
import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONNULL;
import net.siisise.json.JSONReplacer;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONValueP extends ABNFSelect<JSONValue> {

    public JSONValueP(ABNF def, ABNFReg reg, ABNFReg base) {
        super(def, reg, base, "object", "array", "number", "string");
    }

    @Override
    protected JSONValue other(Packet pac) {
        Packet p = JSON8259Reg.FALSE.is(pac);
        if (p != null) {
            return new JSONBoolean(false);
        }
        p = JSON8259Reg.NULL.is(pac);
        if (p != null) {
            return new JSONNULL();
        }
        p = JSON8259Reg.TRUE.is(pac);
        if (p != null) {
            return new JSONBoolean(true);
        }
        return null;
    }

    /**
     *
     * @param obj
     * @param replacer
     * @return
     */
    public static JSONValue valueOf(Object obj, JSONReplacer replacer) {
        if ( obj == null) {
            return new JSONNULL();
        } else if (obj instanceof JSONValue) {
            return (JSONValue) obj;
        } else if ( obj instanceof Boolean ) {
            return new JSONBoolean((Boolean)obj);
        }
        return null;
    }
}
