package net.siisise.json.pointer;

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

    public static class Patch {
        public String op;
        public JSONPointer path;
        public JSONPointer from;
        // StringではなくJSONValueがいい
        public JSON2Value value;
        
        public JSON2Collection apply(JSON2Collection target) { return target; }
        
        public JSON2Value toJSON() {
            op = getClass().getName().substring(3).toLowerCase();
            JSON2Object p = new JSON2Object();
            if ( op != null ) {
                p.putJSON("op",JSON2.valueOf(op));
                throw new UnsupportedOperationException(op);
            }
            if ( path != null ) {
                p.putJSON("path",JSON2.valueOf(path));
            }
            if ( from != null ) {
                p.putJSON("from",JSON2.valueOf(from));
            }
            if ( path != null ) {
                p.putJSON("value",value);
            }
            return p;
        }
    }
    
    public static class CmdAdd extends Patch {
        
        public CmdAdd() { op ="add"; }
        
        @Override
        public JSON2Collection apply(JSON2Collection target) {
            path.add(target, value);
            return target;
        }
    }

    public static class CmdRemove extends Patch {
        
        public CmdRemove() { op = "remove"; }
        
        @Override
        public JSON2Collection apply(JSON2Collection target) {
            path.remove(target);
            return target;
        }
    }
    
    public static class CmdReplace extends Patch {
        
        public CmdReplace() { op = "replace"; }
        
        @Override
        public JSON2Collection apply(JSON2Collection target) {
            path.remove(target);
            path.add(target, value);
            return target;
        }
        
    }

    public static class CmdMove extends Patch {
        
        public CmdMove() { op = "move"; }
        
        @Override
        public JSON2Collection apply(JSON2Collection target) {
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
        public JSON2Collection apply(JSON2Collection target) {
            JSON2Value v = from.get(target);
            v = JSON2.parseWrap(v.toString());
            path.add(target, v);
            return target;
        }
    }

    public static class CmdTest extends Patch {
        
        public CmdTest() { op = "test"; }

        @Override
        public JSON2Collection apply(JSON2Collection target) {
            JSON2Value val1 = path.get(target);
            if (!val1.equals(value)) {
                return null;
            }
            return target;
        }
    }

    JSON2Array<Patch> cmds = new JSON2Array();

    public JSONPatch() {
    }

    public JSONPatch(JSON2Array patchList) {
    
        for (Object patch : patchList) {
            cmds.add(cmd((JSON2Object) JSON2.valueOf(patch)));
        }
    }
    
    /**
     * エラー未実装
     *
     * @param target
     * @return エラーっぽいときはnull
     */
    public JSON2Collection run(JSON2Collection target) {
        JSON2Collection cp = (JSON2Collection) JSON2.parse(target.toString()); // 複製してみたり
        for ( Patch cmd : cmds ) {
            cp = cmd.apply(cp);
        }
        return cp;
    }
    
    public static JSON2Collection run(JSON2Collection target, JSON2Array patchs) {
        JSONPatch p = new JSONPatch(patchs);
        return p.run(target);
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
    
    @Override
    public <T extends JsonStructure> T apply(T target) {
        return run((JSON2Collection) JSON2.valueOf(target)).typeMap(JsonStructure.class);
    }

    @Override
    public JsonArray toJsonArray() {
        return cmds.toJson();

    }
}
