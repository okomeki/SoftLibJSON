package net.siisise.json.pointer;

import java.util.List;
import net.siisise.json.JSON;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONCollection;
import net.siisise.json.JSONValue;

/**
 * RFC 6902 JavaScript Object Notation (JSON) Patch.
 * https://tools.ietf.org/html/rfc6902
 */
public class JSONPatch {

    public String op;
    public JSONPointer path;
    public JSONPointer from;
    // StringではなくJSONValueがいい
    public String value;

    /**
     * エラー未実装
     *
     * @param obj
     * @param patchList
     * @return エラーっぽいときはnull
     */
    public static JSONCollection run(JSONCollection obj, JSONArray patchList) {
        JSONCollection cp = (JSONCollection) JSON.parse(obj.toString());

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
        path.add(obj, JSON.parse(value));
        return obj;
    }

    private JSONCollection remove(JSONCollection obj) {
        path.remove(obj);
        return obj;
    }

    private JSONCollection replace(JSONCollection obj) {
        path.remove(obj);
        path.add(obj, JSON.parse(value));
        return obj;
    }

    private JSONCollection move(JSONCollection obj) {
        JSONValue v = from.get(obj);
        v = JSON.parse(v.toString());
        from.remove(obj);
        path.add(obj, v);
        return obj;
    }

    private JSONCollection copy(JSONCollection obj) {
        JSONValue v = from.get(obj);
        v = JSON.parse(v.toString());
        path.add(obj, v);
        return obj;
    }

    /**
     * まだ
     *
     * @param obj
     * @return 成功すればobj 失敗すればnull 未実装なのでExceptionも返る
     */
    private JSONCollection test(JSONCollection obj) {
        JSONValue val1 = path.get(obj);
        //String txt1 = val1.toString(); // 正規化?
        JSONValue val2 = JSON.parse(value);
        //String txt2 = val2.toString();
        if (!val1.equals(val2)) {
            return null;
        }
        return obj;
    }
}
