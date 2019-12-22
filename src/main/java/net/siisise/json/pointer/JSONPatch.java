package net.siisise.json.pointer;

import java.util.List;
import net.siisise.json.JSON8259Reg;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONCollection;
import net.siisise.json.JSONValue;

/**
 * RFC 6902 JavaScript Object Notation (JSON) Patch.
 * https://tools.ietf.org/html/rfc6902
 */
public class JSONPatch {

    public String op;
    public String path;
    public String from;
    public String value;

    /**
     * エラー未実装
     *
     * @param obj
     * @param patchList
     * @return エラーっぽいときはnull
     */
    public static JSONCollection run(JSONCollection obj, JSONArray patchList) {
        JSONCollection cp = (JSONCollection) JSON8259Reg.parse(obj.toString());
        
        List<JSONValue> ops = patchList.value();
        for (JSONValue patch : ops) {
            JSONPatch p = (JSONPatch) patch.map(JSONPatch.class);
            cp = p.cmd(cp);
        }
        return cp;
    }

    /**
     *
     * @param obj
     * @return エラーはnull (エラー未実装あり)
     */
    public JSONCollection cmd(JSONCollection obj) {
        if ("add".equals(op)) {
            return add(obj);
        } else if ("remove".equals(op)) {
            return remove(obj);
        } else if ("replace".equals(op)) {
            return replace(obj);
        } else if ("move".equals(op)) {
            return move(obj);
        } else if ("copy".equals(op)) {
            return copy(obj);
        } else if ("test".equals(op)) {
            return test(obj);
        }
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private JSONCollection add(JSONCollection obj) {
        obj.add(new JSONPointer(path), value);
        return obj;
    }

    private JSONCollection remove(JSONCollection obj) {
        obj.remove(new JSONPointer(path));
        return obj;
    }

    private JSONCollection replace(JSONCollection obj) {
        obj.remove(new JSONPointer(path));
        obj.add(new JSONPointer(path), value);
        return obj;
    }

    private JSONCollection move(JSONCollection obj) {
        JSONValue v = obj.get(new JSONPointer(from));
        v = JSON8259Reg.parse(v.toString());
        obj.remove(new JSONPointer(from));
        obj.add(new JSONPointer(path), v);
        return obj;
    }

    private JSONCollection copy(JSONCollection obj) {
        JSONValue v = obj.get(new JSONPointer(from));
        v = JSON8259Reg.parse(v.toString());
        obj.add(new JSONPointer(path), v);
        return obj;
    }

    /**
     * まだ
     *
     * @param obj
     * @return 成功すればobj 失敗すればnull 未実装なのでExceptionも返る
     */
    private JSONCollection test(JSONCollection obj) {
        JSONValue val1 = obj.get(new JSONPointer(path));
        //String txt1 = val1.toString(); // 正規化?
        JSONValue val2 = JSON8259Reg.parse(value);
        //String txt2 = val2.toString();
        if (!val1.equals(val2)) {
            return null;
        }
        return obj;
    }
}
