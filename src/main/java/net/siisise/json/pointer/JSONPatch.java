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
package net.siisise.json.pointer;

import java.util.HashSet;
import javax.json.JsonArray;
import javax.json.JsonPatch;
import javax.json.JsonStructure;
import net.siisise.json2.JSON2;
import net.siisise.json2.JSON2Array;
import net.siisise.json2.JSON2Collection;
import net.siisise.json2.JSON2Object;
import net.siisise.json2.JSON2Value;

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
        
        public <T extends JSON2Collection> T apply(T target) { return target; }
        
        public JSON2Object toJSON() { // ない項目を省略するだけ
            //op = getClass().getName().substring(3).toLowerCase();
            JSON2Object p = new JSON2Object();
            if ( op != null ) {
                p.putJSON("op",JSON2.valueOf(op));
            }
            if ( path != null ) {
                p.putJSON("path",JSON2.valueOf(path));
            }
            if ( from != null ) {
                p.putJSON("from",JSON2.valueOf(from));
            }
            if ( value != null ) {
                p.putJSON("value",value);
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
        
        public CmdAdd() { op ="add"; }
        
        @Override
        public <T extends JSON2Collection> T apply(T target) {
            path.add(target, value);
            return target;
        }
    }

    public static class CmdRemove extends Patch {
        
        public CmdRemove() { op = "remove"; }
        
        @Override
        public <T extends JSON2Collection> T apply(T target) {
            path.remove(target);
            return target;
        }
    }
    
    public static class CmdReplace extends Patch {
        
        public CmdReplace() { op = "replace"; }
        
        @Override
        public <T extends JSON2Collection> T apply(T target) {
            path.remove(target);
            path.add(target, value);
            return target;
        }
        
    }

    public static class CmdMove extends Patch {
        
        public CmdMove() { op = "move"; }
        
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
        
        public CmdCopy() { op = "copy"; }

        @Override
        public <T extends JSON2Collection> T apply(T target) {
            JSON2Value v = from.get(target);
            v = JSON2.parseWrap(v.toString());
            path.add(target, v);
            return target;
        }
    }

    public static class CmdTest extends Patch {
        
        public CmdTest() { op = "test"; }

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
     * @param patch
     * @return 
     */
    static Patch cmd(JSON2Object patch) {
        
        String op = (String) patch.get("op");
        Class<?> cls;
        if ( op.equals("add") ) {
            cls = CmdAdd.class;
        } else if ( op.equals("remove")) {
            cls = CmdRemove.class;
        } else if ( op.equals("replace")) {
            cls = CmdReplace.class;
        } else if ( op.equals("move")) {
            cls = CmdMove.class;
        } else if ( op.equals("copy")) {
            cls = CmdCopy.class;
        } else if ( op.equals("test")) {
            cls = CmdTest.class;
        } else {
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }
        return (Patch)patch.typeMap(cls);
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
        for ( Patch cmd : cmds ) {
            target = cmd.apply(target);
        }
        return target;
    }
    
    public static JSON2Collection run(JSON2Collection target, JSON2Array patchs) {
        JSONPatch p = new JSONPatch(patchs);
        return p.apply(target);
    }
    
    @Override
    public <T extends JsonStructure> T apply(T target) {
        Class c = target.getClass();
        JSON2Collection cp = (JSON2Collection) JSON2.valueOf(target);
        for ( Patch cmd : cmds ) {
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
    
    private void mrg( String path, JSONPatch diff ) {
        for ( Patch cmd : diff.cmds ) {
            if ( cmd.from != null ) {
                cmd.from = new JSONPointer(path + cmd.from.toString());
            }
            if ( cmd.path != null ) {
                cmd.path = new JSONPointer(path + cmd.path.toString());
            }
            cmds.add(cmd);
        }
    }
            
    static JSONPatch diffArray( JSON2Array<?> source, JSON2Array<?> target ) {
        JSONPatch patch = new JSONPatch();
        
        JSON2Array obj = source.clone();
        
        for ( int t = 0; t < target.size(); t++ ) {
            Object tv = target.get(t);
            if ( obj.size() > t ) {
                if ( obj.get(t).equals(tv)) {
                    // 一致
                    continue;
                }
                if ( !target.contains(obj.get(t)) ) {
                    patch.diff("/" + t, obj.getJSON(t), target.getJSON(t));
                    obj.setJSON(t, JSON2.copy(target.getJSON(t)));
                    continue;
                }
            }
            if ( obj.contains(tv) ) {
                int idx = obj.indexOf(tv);
                if ( idx < t ) {
                    CmdCopy cp = new CmdCopy();
                    cp.from = new JSONPointer("/" + idx);
                    if ( t >= obj.size() ) {
                        cp.path = new JSONPointer("/-");
                    } else {
                        cp.path = new JSONPointer("/" + t);
                    }
                    patch.cmds.add(cp);
                } else if ( idx > t ) {
                    CmdMove mv = new CmdMove();
                    mv.from = new JSONPointer("/" + idx);
                    if ( t >= obj.size() ) {
                        mv.path = new JSONPointer("/-");
                    } else {
                        mv.path = new JSONPointer("/" + t);
                    }
                    patch.cmds.add(mv);
                }
            } else {
                // ToDo: diff
                
                // add
                CmdAdd ad = new CmdAdd();
                ad.value = JSON2.valueWrap(tv);
                if ( t >= obj.size() ) {
                    ad.path = new JSONPointer("/-");
                } else {
                    ad.path = new JSONPointer("/" + t);
                }
                patch.cmds.add(ad);
            }
            
        }
        while ( obj.size() > target.size() ) {
            CmdRemove rm = new CmdRemove();
            rm.path = new JSONPointer("/" + target.size());
            patch.cmds.add(rm);
        }
        return patch;
    }
    
    static JSONPatch diffObject( JSON2Object source, JSON2Object target ) {
        
        HashSet<String> all = new HashSet<>(target.keySet());
        all.addAll(source.keySet());
        
        JSONPatch patch = new JSONPatch();
        for ( String key : all ) {
            patch.diff( "/" + JSONPointer.encode(key), source.getJSON(key), target.getJSON(key));
        }
        
        return patch;
    }

    /**
     * add remove replace のみの差分を出力する
     * move copy は未対応
     * @param source
     * @param target
     * @return 
     */
    public static JSONPatch diff(JSON2Value source, JSON2Value target) {
        JSONPatch patch = new JSONPatch();
        
        if ( source == null ) {
            if ( target != null ) {
                CmdAdd p = new CmdAdd();
                p.value = target;
                p.path = new JSONPointer("");
                patch.cmds.add(p);
            }
        } else if ( target == null ) {
            CmdRemove d = new CmdRemove();
            d.path = new JSONPointer("");
            patch.cmds.add(d);
        } else if ( !source.equals(target)) {
            // object と array は別でなんとかする
            if ( source instanceof JSON2Array && target instanceof JSON2Array ) {
                JSONPatch subPatch = diffArray((JSON2Array)source, (JSON2Array)target);
                patch.mrg("", subPatch);
            } else if ( source instanceof JSON2Object && target instanceof JSON2Object ) {
                JSONPatch subPatch = diffObject((JSON2Object)source, (JSON2Object)target);
                patch.mrg("", subPatch);
            } else {
                CmdReplace r = new CmdReplace();
                r.path = new JSONPointer("");
                r.value = target;
                patch.cmds.add(r);
            }
        }
        return patch;
    }
    
    public void diff(String path, JSON2Value source, JSON2Value target) {
        JSONPatch p = diff(source, target);
        mrg(path, p);
    }
}
