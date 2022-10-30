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
package net.siisise.json.base;

import java.util.HashSet;
import java.util.Map;
import net.siisise.json.JSON;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONCollection;
import net.siisise.json.JSONObject;
import net.siisise.json.JSONPatch;
import net.siisise.json.JSONPointer;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONBasePatch {
    
    public static class Patch implements Cloneable {

        public String op;
        public JSONPointer path;
        public JSONPointer from;
        // StringではなくJSONValueがいい
        public JSONValue value;

        public <T extends JSONCollection> T apply(T target) {
            return target;
        }

        public JSONObject toJSON() { // ない項目を省略するだけ
            //op = getClass().getName().substring(3).toLowerCase();
            JSONObject p = new JSONObject();
            if (op != null) {
                p.put("op", op);
            }
            if (path != null) {
                p.putJSON("path", JSON.valueOf(path));
            }
            if (from != null) {
                p.putJSON("from", JSON.valueOf(from));
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
            c.value = JSON.copy(value);
            return c;
        }
    }

    public static class CmdAdd extends Patch {

        public CmdAdd() {
            op = "add";
        }

        @Override
        public <T extends JSONCollection> T apply(T target) {
            path.add(target, value);
            return target;
        }
    }

    public static class CmdRemove extends Patch {

        public CmdRemove() {
            op = "remove";
        }

        @Override
        public <T extends JSONCollection> T apply(T target) {
            path.remove(target);
            return target;
        }
    }

    public static class CmdReplace extends Patch {

        public CmdReplace() {
            op = "replace";
        }

        @Override
        public <T extends JSONCollection> T apply(T target) {
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
        public <T extends JSONCollection> T apply(T target) {
            JSONValue v = from.get(target);
            v = JSON.copy(v);
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
        public <T extends JSONCollection> T apply(T target) {
            JSONValue v = from.get(target);
            v = JSON.copy(v);
            path.add(target, v);
            return target;
        }
    }

    public static class CmdTest extends Patch {

        public CmdTest() {
            op = "test";
        }

        @Override
        public <T extends JSONCollection> T apply(T target) {
            JSONValue val1 = path.get(target);
            if (!val1.equals(value)) {
                return null;
            }
            return target;
        }
    }

    public final JSONArray<Patch> cmds = new JSONArray();

    public JSONBasePatch() {
    }

    public JSONBasePatch(JSONArray patchList) {
        for (Object patch : patchList) {
            cmds.add(cmd((JSONObject) JSON.valueOf(patch)));
        }
    }

    /**
     * JSONPatchBuilderっぽい機能
     *
     * @param patch
     * @return
     */
    static Patch cmd(JSONObject patch) {

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

    public void add(JSONPointer path, JSONValue val) {
        CmdAdd cmd = new CmdAdd();
        cmd.path = path;
        cmd.value = val;
        cmds.add(cmd);
    }

    public void add(String path, Object val) {
        add(new JSONPointer(path), JSON.valueOf(val));
    }

    public void remove(JSONPointer path) {
        CmdRemove cmd = new CmdRemove();
        cmd.path = path;
        cmds.add(cmd);
    }

    public void replace(JSONPointer path, JSONValue val) {
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
     *
     * @param target 元
     * @param patchs 差分
     * @return 結果
     */
    public static JSONCollection run(JSONCollection target, JSONArray patchs) {
        JSONPatch p = new JSONPatch(patchs);
        return p.apply(target);
    }

    public JSONValue toJSON() {
        return cmds.clone();
    }

    protected void mrg(String path, JSONBasePatch diff) {
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

    private static int fullCount(JSONObject<?> o, JSONValue v) {
        int c = 0;

        for (String key : o.keySet()) {
            JSONValue ov = o.getJSON(key);
            if (ov == v || (v != null && ov.equals(v))) {
                c++;
            } else if (ov instanceof JSONArray) {
                c += fullCount((JSONArray) ov, v);
            } else if (ov instanceof JSONObject) {
                c += fullCount((JSONObject) ov, v);
            }
        }
        return c;
    }

    private static int fullCount(JSONArray o, JSONValue v) {
        int c = 0;

        for (Object d : o) {
            JSONValue ov = JSON.valueOf(d);
            if (ov == v || (v != null && ov.equals(v))) {
                c++;
            } else if (ov instanceof JSONArray) {
                c += fullCount((JSONArray) ov, v);
            } else if (ov instanceof JSONObject) {
                c += fullCount((JSONObject) ov, v);
            }
        }
        return c;
    }

    private static int count(JSONArray o, Object v) {
        int c = 0;

        for (int n = 0; n < o.size(); n++) {
            Object ov = o.get(n);
            if (ov == v || (v != null && v.equals(ov))) { // null 対策
                c++;
            }
        }
        return c;
    }

    private static int count(JSONArray o, JSONValue v) {
        int c = 0;

        for (int n = 0; n < o.size(); n++) {
            JSONValue ov = o.getJSON(n);
            if (ov == v || (v != null && ov.equals(v))) { // null 対策
                c++;
            }
        }
        return c;
    }

    private static int indexOf(JSONArray obj, Object val, int start) {
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
    static JSONPatch diffArray(JSONArray<?> source, JSONArray<?> target) {
        JSONPatch patch = new JSONPatch();
        // ソース側をターゲットと同じになるまでつつく
        JSONArray src = source.clone();

        for (int t = 0; t < target.size(); t++) {
//            System.out.println("S:" + src.toJSON() + " T:" + target.toJSON());
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
                        patch.add(new JSONPointer("/" + t), JSON.valueOf(newVal));
                        src.add(t, newVal);
                    } else {
                        int si = src.indexOf(newVal);
                        patch.copy(new JSONPointer("/" + si), new JSONPointer("/" + t));
                        JSONValue v = JSON.copy(src.getJSON(si));
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
                        src.addJSON(t, JSON.copy(src.getJSON(i)));
                    } else {
                        patch.diff("/" + t, src.getJSON(t), target.getJSON(t));
                        src.setJSON(t, JSON.copy(target.getJSON(t)));
                    }
                } else {
                    patch.diff("/" + t, src.getJSON(t), target.getJSON(t));
                    src.setJSON(t, JSON.copy(target.getJSON(t)));
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
                    src.addJSON(t, JSON.copy(src.getJSON(idx)));
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
                patch.add(path, JSON.valueWrap(newVal));
            }

        }
        while (src.size() > target.size()) {
            patch.remove(new JSONPointer("/" + target.size()));
        }
        return patch;
    }

    static JSONPatch diffObject(JSONObject source, JSONObject target) {

        JSONObject<?> obj = source.clone();

        HashSet<String> all = new HashSet<>(target.keySet());
        all.addAll(source.keySet());
        HashSet<String> rm = new HashSet<>(source.keySet());
        rm.removeAll(target.keySet());

        JSONPatch patch = new JSONPatch();
        for (String key : all) {  // ランダム
            String encKey = "/" + JSONPointer.encode(key);
            Object tgt = target.get(key);
            JSONValue srcjson = obj.getJSON(key);
            JSONValue tgtjson = target.getJSON(key);
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
                    obj.putJSON(key, JSON.copy(tgtjson));
                } else {

                    patch.copy(new JSONPointer("/" + JSONPointer.encode(es[0].getKey())), new JSONPointer(encKey));
                    obj.putJSON(key, JSON.copy(tgtjson));
                }
            } else {
                patch.diff(encKey, srcjson, tgtjson);
            }
        }
        for (String key : rm) {
            String encKey = "/" + JSONPointer.encode(key);
            JSONValue srcjson = obj.getJSON(key);
            JSONValue tgtjson = target.getJSON(key);
            patch.diff(encKey, srcjson, tgtjson);
        }

        return patch;
    }

    /**
     * add remove replace のみの差分を出力する move copy は未対応
     *
     * @param source
     * @param target
     * @param patch
     * @return
     */
    public static JSONBasePatch diff(JSONValue source, JSONValue target, JSONBasePatch patch) {

        if (source == null) {
            if (target != null) {
                patch.add(new JSONPointer(""), JSON.copy(target));
            }
        } else if (target == null) {
            patch.remove(new JSONPointer(""));
        } else if (!source.equals(target)) {
            // object と array は別でなんとかする
            if (source instanceof JSONArray && target instanceof JSONArray) {
                JSONPatch subPatch = diffArray((JSONArray) source, (JSONArray) target);
                patch.mrg("", subPatch);
            } else if (source instanceof JSONObject && target instanceof JSONObject) {
                JSONPatch subPatch = diffObject((JSONObject) source, (JSONObject) target);
                patch.mrg("", subPatch);
            } else {
                patch.replace(new JSONPointer(""), JSON.copy(target));
            }
        }
        return patch;
    }

    public void diff(String path, JSONValue source, JSONValue target) {
        JSONBasePatch p = diff(source, target, new JSONBasePatch());
        mrg(path, p);
    }

}
