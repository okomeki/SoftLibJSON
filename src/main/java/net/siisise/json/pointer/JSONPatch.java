package net.siisise.json.pointer;

import java.util.List;
import net.siisise.json.JSON8259Reg;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONCollection;
import net.siisise.json.JSONValue;

/**
 * RFC 6902 JSON Patch
 *
 * @author okome
 */
public class JSONPatch {

    public static class Patch {

        public String op;
        public String path;
        public String from;
        public String value;
    }

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
            Patch p = (Patch) patch.map(Patch.class);
            cp = cmd(cp, p);
        }
        return cp;
    }

    /**
     *
     * @param obj
     * @param patch
     * @return エラーはnull (エラー未実装あり)
     */
    private static JSONCollection cmd(JSONCollection obj, Patch patch) {
        if ("add".equals(patch.op)) {
            return add(obj, patch);
        } else if ("remove".equals(patch.op)) {
            return remove(obj, patch);
        } else if ("replace".equals(patch.op)) {
            return replace(obj, patch);
        } else if ("move".equals(patch.op)) {
            return move(obj, patch);
        } else if ("copy".equals(patch.op)) {
            return copy(obj, patch);
        } else if ("test".equals(patch.op)) {
            return test(obj, patch);
        }
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    private static JSONCollection add(JSONCollection obj, Patch patch) {
        obj.add(new JSONPointer(patch.path), patch.value);
        return obj;
    }

    private static JSONCollection remove(JSONCollection obj, Patch patch) {
        obj.remove(new JSONPointer(patch.path));
        return obj;
    }

    private static JSONCollection replace(JSONCollection obj, Patch patch) {
        obj.remove(new JSONPointer(patch.path));
        obj.add(new JSONPointer(patch.path), patch.value);
        return obj;
    }

    private static JSONCollection move(JSONCollection obj, Patch patch) {
        JSONValue v = obj.get(new JSONPointer(patch.from));
        v = JSON8259Reg.parse(v.toString());
        obj.remove(new JSONPointer(patch.from));
        obj.add(new JSONPointer(patch.path), v);
        return obj;
    }

    private static JSONCollection copy(JSONCollection obj, Patch patch) {
        JSONValue v = obj.get(new JSONPointer(patch.from));
        v = JSON8259Reg.parse(v.toString());
        obj.add(new JSONPointer(patch.path), v);
        return obj;
    }

    /**
     * まだ
     *
     * @param obj
     * @param patch
     * @return 成功すればobj 失敗すればnull 未実装なのでExceptionも返る
     */
    private static JSONCollection test(JSONCollection obj, Patch patch) {
        JSONValue val1 = obj.get(new JSONPointer(patch.path));
        //String txt1 = val1.toString(); // 正規化?
        JSONValue val2 = JSON8259Reg.parse(patch.value);
        //String txt2 = val2.toString();
        if (!val1.equals(val2)) {
            return null;
        }
        return obj;
    }
}
