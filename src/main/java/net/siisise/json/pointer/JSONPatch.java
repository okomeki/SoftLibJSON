package net.siisise.json.pointer;

import net.siisise.json2.JSON2;
import net.siisise.json2.JSON2Array;
import net.siisise.json2.JSON2Collection;
import net.siisise.json2.JSON2Object;
import net.siisise.json2.JSON2Value;

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
    public static JSON2Collection run(JSON2Collection obj, JSON2Array patchList) {
        JSON2Collection cp = (JSON2Collection) JSON2.parse(obj.toString());

        for (Object patch : patchList) {
            JSONPatch p = (JSONPatch) ((JSON2Object)patch).typeMap(JSONPatch.class);
            cp = p.cmd(cp);
        }
        return cp;
    }

    /**
     *
     * @param obj
     * @return エラーはnull (エラー未実装あり)
     */
    public JSON2Collection cmd(JSON2Collection obj) {
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

    /** ToDo: wrapいらないかも */
    private JSON2Collection add(JSON2Collection obj) {
        path.add(obj, JSON2.valueWrap(JSON2.parse(value)));
        return obj;
    }

    private JSON2Collection remove(JSON2Collection obj) {
        path.remove(obj);
        return obj;
    }

    private JSON2Collection replace(JSON2Collection obj) {
        path.remove(obj);
        path.add(obj, JSON2.valueWrap(JSON2.parse(value)));
        return obj;
    }

    private JSON2Collection move(JSON2Collection obj) {
        JSON2Value v = from.get(obj);
        v = JSON2.valueWrap(JSON2.parse(v.toString()));
        from.remove(obj);
        path.add(obj, v);
        return obj;
    }

    private JSON2Collection copy(JSON2Collection obj) {
        JSON2Value v = from.get(obj);
        v = JSON2.valueWrap(JSON2.parse(v.toString()));
        path.add(obj, v);
        return obj;
    }

    /**
     * まだ
     *
     * @param obj
     * @return 成功すればobj 失敗すればnull 未実装なのでExceptionも返る
     */
    private JSON2Collection test(JSON2Collection obj) {
        JSON2Value val1 = path.get(obj);
        //String txt1 = val1.toString(); // 正規化?
        JSON2Value val2 = JSON2.valueWrap(JSON2.parse(value));
        //String txt2 = val2.toString();
        if (!val1.equals(val2)) {
            return null;
        }
        return obj;
    }
}
