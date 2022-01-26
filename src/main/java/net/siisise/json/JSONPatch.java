/*
 * Copyright 2022 okome.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.siisise.json;

import java.util.HashSet;
import java.util.Map;
import javax.json.JsonArray;
import javax.json.JsonPatch;
import javax.json.JsonStructure;

/**
 * RFC 6902 JavaScript Object Notation (JSON) Patch.
 * https://tools.ietf.org/html/rfc6902
 */
public class JSONPatch implements JsonPatch {

    public static class Patch implements Cloneable {

        public String op;
        public JSONPointer path;
        public JSONPointer from;
        // StringではなくJSONValueがいい
        public JSON2Value value;

        public <T extends JSON2Collection> T apply(T target) {
            return target;
        }

        public JSON2Object toJSON() { // ない項目を省略するだけ
            //op = getClass().getName().substring(3).toLowerCase();
            JSON2Object p = new JSON2Object();
            if (op != null) {
                p.put("op", op);
            }
            if (path != null) {
                p.putJSON("path", JSON2.valueOf(path));
            }
            if (from != null) {
                p.putJSON("from", JSON2.valueOf(from));
            }
            if (value != null) {
                p.putJSON("value", value);
            }
            return p;
        }

        @Override
        public Patch clone() throws CloneNotSupportedException {
            Patch c;
            c = (Patch) super.clone();
            c.value = JSON2.copy(value);
            return c;
        }
    }

    public static class CmdAdd extends Patch {

        public CmdAdd() {
            op = "add";
        }

        @Override
        public <T extends JSON2Collection> T apply(T target) {
            path.add(target, value);
            return target;
        }
    }

    public static class CmdRemove extends Patch {

        public CmdRemove() {
            op = "remove";
        }

        @Override
        public <T extends JSON2Collection> T apply(T target) {
            path.remove(target);
            return target;
        }
    }

    public static class CmdReplace extends Patch {

        public CmdReplace() {
            op = "replace";
        }

        @Override
        public <T extends JSON2Collection> T apply(T target) {
            path.remove(target);
            path.add(target, value);
            return target;
        }

    }

    public static class CmdMove extends Patch {

        public CmdMove() {
            op = "move";
        }

        @Override
        public <T extends JSON2Collection> T apply(T target) {
            JSON2Value v = from.get(target);
            v = JSON2.parseWrap(v.toString());
            from.remove(target);
            path.add(target, v);
            return target;
        }

    }

    public static class CmdCopy extends Patch {

        public CmdCopy() {
            op = "copy";
        }

        @Override
        public <T extends JSON2Collection> T apply(T target) {
            JSON2Value v = from.get(target);
            v = JSON2.parseWrap(v.toString());
            path.add(target, v);
            return target;
        }
    }

    public static class CmdTest extends Patch {

        public CmdTest() {
            op = "test";
        }

        @Override
        public <T extends JSON2Collection> T apply(T target) {
            JSON2Value val1 = path.get(target);
            if (!val1.equals(value)) {
                return null;
            }
            return target;
        }
    }

    final JSON2Array<Patch> cmds = new JSON2Array();

    public JSONPatch() {
    }

    public JSONPatch(JSON2Array patchList) {

        for (Object patch : patchList) {
            cmds.add(cmd((JSON2Object) JSON2.valueOf(patch)));
        }
    }

    /**
     * JSONPatchBuilderっぽい機能
     *
     * @param patch
     * @return
     */
    static Patch cmd(JSON2Object patch) {

        String op = (String) patch.get("op");
        Class<?> cls;
        if (op.equals("add")) {
            cls = CmdAdd.class;
        } else if (op.equals("remove")) {
            cls = CmdRemove.class;
        } else if (op.equals("replace")) {
            cls = CmdReplace.class;
        } else if (op.equals("move")) {
            cls = CmdMove.class;
        } else if (op.equals("copy")) {
            cls = CmdCopy.class;
        } else if (op.equals("test")) {
            cls = CmdTest.class;
        } else {
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }
        return (Patch) patch.typeMap(cls);
    }

    public void add(JSONPointer path, JSON2Value val) {
        CmdAdd cmd = new CmdAdd();
        cmd.path = path;
        cmd.value = val;
        cmds.add(cmd);
    }

    public void add(String path, Object val) {
        add(new JSONPointer(path), JSON2.valueOf(val));
    }

    public void remove(JSONPointer path) {
        CmdRemove cmd = new CmdRemove();
        cmd.path = path;
        cmds.add(cmd);
    }

    public void replace(JSONPointer path, JSON2Value val) {
        CmdReplace cmd = new CmdReplace();
        cmd.path = path;
        cmd.value = val;
        cmds.add(cmd);
    }

    public void move(JSONPointer from, JSONPointer path) {
        CmdMove cmd = new CmdMove();
        cmd.from = from;
        cmd.path = path;
        cmds.add(cmd);
    }

    public void copy(JSONPointer from, JSONPointer path) {
        CmdCopy cmd = new CmdCopy();
        cmd.from = from;
        cmd.path = path;
        cmds.add(cmd);
    }

    /**
     * エラー未実装
     *
     * @param <T> 型変換対策
     * @param target 元は可変
     * @return target処理後の複製? エラーっぽいときはnull
     */
    public <T extends JSON2Collection> T apply(T target) {
        target = (T) target.clone();
        for (Patch cmd : cmds) {
            target = cmd.apply(target);
        }
        return target;
    }

    /**
     *
     * @param target 元
     * @param patchs 差分
     * @return 結果
     */
    public static JSON2Collection run(JSON2Collection target, JSON2Array patchs) {
        JSONPatch p = new JSONPatch(patchs);
        return p.apply(target);
    }

    @Override
    public <T extends JsonStructure> T apply(T target) {
        Class c = target.getClass();
        JSON2Collection cp = (JSON2Collection) JSON2.valueOf(target);
        for (Patch cmd : cmds) {
            cp = cmd.apply(cp);
        }
        return cp.typeMap(c);
    }

    @Override
    public JsonArray toJsonArray() {
        return cmds.toJson();
    }

    public JSON2Value toJSON() {
        return cmds.clone();
    }

    private void mrg(String path, JSONPatch diff) {
        for (Patch cmd : diff.cmds) {
            if (cmd.from != null) {
                cmd.from = new JSONPointer(path + cmd.from.toString());
            }
            if (cmd.path != null) {
                cmd.path = new JSONPointer(path + cmd.path.toString());
            }
            cmds.add(cmd);
        }
    }

    private static int fullCount(JSON2Object<?> o, JSON2Value v) {
        int c = 0;

        for (String key : o.keySet()) {
            JSON2Value ov = o.getJSON(key);
            if (ov == v || (v != null && ov.equals(v))) {
                c++;
            } else if (ov instanceof JSON2Array) {
                c += fullCount((JSON2Array) ov, v);
            } else if (ov instanceof JSON2Object) {
                c += fullCount((JSON2Object) ov, v);
            }
        }
        return c;
    }

    private static int fullCount(JSON2Array o, JSON2Value v) {
        int c = 0;

        for (Object d : o) {
            JSON2Value ov = JSON2.valueOf(d);
            if (ov == v || (v != null && ov.equals(v))) {
                c++;
            } else if (ov instanceof JSON2Array) {
                c += fullCount((JSON2Array) ov, v);
            } else if (ov instanceof JSON2Object) {
                c += fullCount((JSON2Object) ov, v);
            }
        }
        return c;
    }

    private static int count(JSON2Array o, Object v) {
        int c = 0;

        for (int n = 0; n < o.size(); n++) {
            Object ov = o.get(n);
            if (ov == v || (v != null && v.equals(ov))) { // null 対策
                c++;
            }
        }
        return c;
    }

    private static int count(JSON2Array o, JSON2Value v) {
        int c = 0;

        for (int n = 0; n < o.size(); n++) {
            JSON2Value ov = o.getJSON(n);
            if (ov == v || (v != null && ov.equals(v))) { // null 対策
                c++;
            }
        }
        return c;
    }

    private static int indexOf(JSON2Array obj, Object val, int start) {
        for (int i = start; i < obj.size(); i++) {
            if (val.equals(obj.get(i))) {
                return i;
            }
        }
        return -1;
    }

    /**
     * 配列の差分取り とり方次第
     *
     * @param source 元
     * @param target 先
     * @return 差分
     */
    static JSONPatch diffArray(JSON2Array<?> source, JSON2Array<?> target) {
        JSONPatch patch = new JSONPatch();
        // ソース側をターゲットと同じになるまでつつく
        JSON2Array src = source.clone();

        for (int t = 0; t < target.size(); t++) {
//            System.out.println("S:" + src.toString() + " T:" + target.toString());
//            System.out.println(t);
            Object newVal = target.get(t);
            // 変換先データの数
            int scount = count(src, newVal);
            int tcount = count(target, newVal);
//            System.out.println(" SC:" + scount + "  TC:" + tcount);
            int oscount;
            int otcount;
            if (src.size() > t) {
                Object sv = src.get(t);
                if (sv == newVal || sv.equals(newVal)) {
                    // 一致
                    continue;
                }
                // 元データの数
                oscount = count(src, sv);
                otcount = count(target, sv);
//                System.out.println("OSC:" + oscount + " OTC:" + otcount);
                if (oscount == otcount) {
                    //add / copy
                    if (scount == 0) {
                        patch.add(new JSONPointer("/" + t), JSON2.valueOf(newVal));
                        src.add(t, newVal);
                    } else {
                        int si = src.indexOf(newVal);
                        patch.copy(new JSONPointer("/" + si), new JSONPointer("/" + t));
                        JSON2Value v = JSON2.copy(src.getJSON(si));
                        src.addJSON(t, v);
                    }
                    continue;
                }
                if (scount >= tcount) {
                    if (oscount >= otcount) {
                        patch.remove(new JSONPointer("/" + t));
                        src.remove(t);
                        t--;
                        continue;
                    }
                    int si = indexOf(src, newVal, t);
                    int ti = indexOf(target, sv, t);
//                    System.out.println("si:" + si);
//                    System.out.println("ti:" + ti);
                    if (si < ti && si == t + 1) {
                        patch.move(new JSONPointer("/" + t), new JSONPointer("/" + ti));
                        Object v = src.remove(t);
                        src.add(ti, v);
                    } else {
                        patch.move(new JSONPointer("/" + si), new JSONPointer("/" + t));
                        Object v = src.remove(si);
                        src.add(t, v);
                    }
                    continue;
                }
                if (oscount > otcount) { // 元が多いので減らす diff または remove 上書き可
                    if (scount < tcount) { // 複製先は残したい
                        int i = src.indexOf(newVal);
                        patch.copy(new JSONPointer("/" + i), new JSONPointer("/" + t)); // copyか
                        src.addJSON(t, JSON2.copy(src.getJSON(i)));
                    } else {
                        patch.diff("/" + t, src.getJSON(t), target.getJSON(t));
                        src.setJSON(t, JSON2.copy(target.getJSON(t)));
                    }
                } else {
                    patch.diff("/" + t, src.getJSON(t), target.getJSON(t));
                    src.setJSON(t, JSON2.copy(target.getJSON(t)));
                    continue;
                }
            }
            // 古い処理 あとで減らす
            if (src.contains(newVal)) {
                int idx = src.indexOf(newVal);

                if (src.size() >= t) {
                    Object oval = src.get(t);
                    oscount = count(src, newVal);
                    otcount = count(target, newVal);
                }
                if (scount < tcount) {
                    JSONPointer path;
                    if (t >= src.size()) {
                        path = new JSONPointer("/-");
                    } else {
                        path = new JSONPointer("/" + t);
                    }
                    patch.copy(new JSONPointer("/" + idx), path);
                    src.addJSON(t, JSON2.copy(src.getJSON(idx)));
                    scount++;
                } else {
                    idx = src.subList(t, src.size()).indexOf(newVal) + t;
                    // 元と数が同じときだけにしたい
                    String path;
                    if (t >= src.size()) {
                        path = "/-";
                    } else {
                        path = "/" + t;
                    }
                    patch.move(new JSONPointer("/" + idx), new JSONPointer(path));
                    src.addJSON(t, src.removeJSON("" + idx));
                }
            } else {
                // ToDo: diff

                // add
                String path;
                if (t >= src.size()) {
                    path = "/-";
                } else {
                    path = "/" + t;
                }
                patch.add(path, JSON2.valueWrap(newVal));
            }

        }
        while (src.size() > target.size()) {
            patch.remove(new JSONPointer("/" + target.size()));
        }
        return patch;
    }

    static JSONPatch diffObject(JSON2Object source, JSON2Object target) {

        JSON2Object<?> obj = source.clone();

        HashSet<String> all = new HashSet<>(target.keySet());
        all.addAll(source.keySet());
        HashSet<String> rm = new HashSet<>(source.keySet());
        rm.removeAll(target.keySet());

        JSONPatch patch = new JSONPatch();
        for (String key : all) {  // ランダム
            String encKey = "/" + JSONPointer.encode(key);
            Object tgt = target.get(key);
            JSON2Value srcjson = obj.getJSON(key);
            JSON2Value tgtjson = target.getJSON(key);
            if ((srcjson == null || !srcjson.equals(tgtjson)) && obj.containsValue(tgt)) { // コピーもとあり
                Map.Entry<String, ?>[] es = (Map.Entry<String, ?>[]) obj.entrySet().stream().filter(e -> e.getValue().equals(tgt)).toArray();
                Map.Entry<String, ?> r = null;
                for (Map.Entry<String, ?> e : es) {
                    if (rm.contains(e.getKey())) {
                        r = e;
                    }
                }
                if (r != null) {
                    rm.remove(r.getKey());
                    patch.move(new JSONPointer("/" + JSONPointer.encode(r.getKey())), new JSONPointer(encKey));
                    obj.remove(r.getKey());
                    obj.putJSON(key, JSON2.copy(tgtjson));
                } else {

                    patch.copy(new JSONPointer("/" + JSONPointer.encode(es[0].getKey())), new JSONPointer(encKey));
                    obj.putJSON(key, JSON2.copy(tgtjson));
                }
            } else {
                patch.diff(encKey, srcjson, tgtjson);
            }
        }
        for (String key : rm) {
            String encKey = "/" + JSONPointer.encode(key);
            JSON2Value srcjson = obj.getJSON(key);
            JSON2Value tgtjson = target.getJSON(key);
            patch.diff(encKey, srcjson, tgtjson);
        }

        return patch;
    }

    /**
     * add remove replace のみの差分を出力する move copy は未対応
     *
     * @param source
     * @param target
     * @return
     */
    public static JSONPatch diff(JSON2Value source, JSON2Value target) {
        JSONPatch patch = new JSONPatch();

        if (source == null) {
            if (target != null) {
                patch.add(new JSONPointer(""), JSON2.copy(target));
            }
        } else if (target == null) {
            patch.remove(new JSONPointer(""));
        } else if (!source.equals(target)) {
            // object と array は別でなんとかする
            if (source instanceof JSON2Array && target instanceof JSON2Array) {
                JSONPatch subPatch = diffArray((JSON2Array) source, (JSON2Array) target);
                patch.mrg("", subPatch);
            } else if (source instanceof JSON2Object && target instanceof JSON2Object) {
                JSONPatch subPatch = diffObject((JSON2Object) source, (JSON2Object) target);
                patch.mrg("", subPatch);
            } else {
                patch.replace(new JSONPointer(""), JSON2.copy(target));
            }
        }
        return patch;
    }

    public void diff(String path, JSON2Value source, JSON2Value target) {
        JSONPatch p = diff(source, target);
        mrg(path, p);
    }
}
