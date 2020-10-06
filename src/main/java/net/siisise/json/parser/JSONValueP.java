package net.siisise.json.parser;

import net.siisise.abnf.ABNF;
import net.siisise.abnf.ABNFReg;
import net.siisise.abnf.parser.ABNFSelect;
import net.siisise.io.FrontPacket;
import net.siisise.json.JSON8259Reg;
import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONNULL;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONValueP extends ABNFSelect<JSONValue> {

    public JSONValueP(ABNF def, ABNFReg reg, ABNFReg base) {
        super(def, reg, base, "object", "array", "number", "string");
    }

    @Override
    protected JSONValue other(FrontPacket pac) {
        FrontPacket p = JSON8259Reg.FALSE.is(pac);
        if (p != null) {
            return JSONBoolean.FALSE;
        }
        p = JSON8259Reg.NULL.is(pac);
        if (p != null) {
            return JSONNULL.NULL;
        }
        p = JSON8259Reg.TRUE.is(pac);
        if (p != null) {
            return JSONBoolean.TRUE;
        }
        return null;
    }
}
