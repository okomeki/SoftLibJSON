package net.siisise.json.pointer;

import java.nio.charset.MalformedInputException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonPointer;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import net.siisise.abnf.AbstractABNF;
import net.siisise.io.Packet;
import net.siisise.json.JSON;
import net.siisise.json.JSONCollection;
import net.siisise.json.JSONValue;
import net.siisise.json2.JSON2;
import net.siisise.json2.JSON2Collection;
import net.siisise.json2.JSON2Value;

/**
 * RFC 6901 JSON Pointer
 *
 */
public class JSONPointer implements JsonPointer {

    String[] path;

    public JSONPointer(String escapedPath) {
        if (!JSONPointerReg.jsonPointer.eq(escapedPath)) {
            throw new java.lang.UnsupportedOperationException();
        }
        this.path = escapedPath.split("/");
    }

    JSONPointer(List<Packet> lp) {
        path = new String[lp.size() + 1];
        path[0] = "";
        int i = 1;
        for (Packet p : lp) {
            path[i] = AbstractABNF.str(p);
            if (!JSONPointerReg.referenceToken.eq(path[i++])) {
                throw new java.lang.UnsupportedOperationException();
            }
        }
    }

    public void add(JSONCollection target, JSONValue value) {
        ColKey vp = step(target);
        vp.coll.addJSON(vp.key, value);
    }

    /** wrap いらないかも */
    public void add(JSON2Collection target, JSON2Value value) {
        Col2Key vp = step(target);
        vp.coll.addJSON(vp.key, value);
    }

    public JSONValue remove(JSONCollection target) {
        ColKey vp = step(target);
        return vp.coll.removeJSON(vp.key);
    }

    public JSON2Value remove(JSON2Collection target) {
        Col2Key vp = step(target);
        return vp.coll.removeJSON(vp.key);
    }

    public JSONValue get(JSONCollection target) {
        return step((JSONValue)target,false).val;
        //return obj.get(this);
    }

    public JSON2Value get(JSON2Collection target) {
        return step((JSON2Value)target,false).val;
        //return obj.get(this);
    }

    public void set(JSONCollection target, Object value) {
        ColKey vp = step(target);
        vp.coll.setJSON(vp.key, JSON.valueOf(value));
    }

    public void set(JSON2Collection target, Object value) {
        Col2Key vp = step(target);
        vp.coll.setJSON(vp.key, JSON2.valueOf(value));
    }

    public void replace(JSONCollection target, Object value) {
        ColKey vp = step(target);
        vp.coll.removeJSON(vp.key);
        vp.coll.addJSON(vp.key, JSON.valueOf(value));
    }

    public void replace(JSON2Collection target, Object value) {
        Col2Key vp = step(target);
        vp.coll.removeJSON(vp.key);
        vp.coll.addJSON(vp.key, JSON2.valueOf(value));
    }

    private static class ColKey {
        private JSONCollection coll;
        private String key;
    }

    private static class Col2Key {
        private JSON2Collection coll;
        private String key;
    }

    private static class JsonColKey {
        private JsonStructure coll;
        private String key;
    }

    public JSONPointer sub() {
        List<Packet> lp = new ArrayList<>();
        if (path.length <= 1) {
            return null;
        }
        for (int i = 2; i < path.length; i++) {
            lp.add(AbstractABNF.pac(path[i]));
        }
        return new JSONPointer(lp);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder(100);
        for (int i = 0; i < path.length; i++) {
            if (i != 0) {
                sb.append("/");
            }
            sb.append(path[i]);
        }
        return sb.toString();
    }

    public String[] toDecodeString() {
        String[] dec = new String[path.length];
        for (int i = 0; i < path.length; i++) {
            try {
                dec[i] = decode(path[i]);
            } catch (MalformedInputException ex) {
                throw new java.lang.UnsupportedOperationException();
//                Logger.getLogger(JSONPointer.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return dec;
    }

    /**
     * RFC 3986
     *
     * @return
     */
    String toURIEncode() {
        StringBuilder sb = new StringBuilder();
        List<String> n = Arrays.asList(path);
        n.remove(0);
        for (String s : n) {
            sb.append("/");
            try {
                sb.append(urlEnc(decode(s)));
            } catch (MalformedInputException ex) {
                throw new java.lang.UnsupportedOperationException();
            }
        }
        return sb.toString();
    }

    static String decode(String str) throws MalformedInputException {
        StringBuilder sb = new StringBuilder(100);
        StringBuilder src = new StringBuilder(str);
        char c;
        while (src.length() > 0) {
            c = src.charAt(0);
            if (c == '~') {
                switch (src.charAt(1)) {
                    case '0':
                        c = '~';
                        break;
                    case '1':
                        c = '/';
                        break;
                    default:
                        throw new java.nio.charset.MalformedInputException(0);
                }
                src.delete(0, 2);
            } else {
                src.deleteCharAt(0);
            }
            sb.append(c);
        }
        return sb.toString();
    }

    /**
     * utf-16? RFC 3986
     *
     * @param str
     * @return
     */
    static String urlEnc(String str) {
        char[] chs = str.toCharArray();
        StringBuilder sb = new StringBuilder(str.length() * 2);
        for (char ch : chs) {
            if (ch < 0x20) {
                sb.append("%");
                sb.append(Integer.toHexString(0x100 + ch).substring(1));
            } else {
                sb.append(ch);

            }
        }
        return sb.toString();
    }

    @Override
    public <T extends JsonStructure> T add(T target, JsonValue value) {
        JsonColKey vp = step(target);
        if ( vp.coll instanceof JsonArray ) {
            ((JsonArray)vp.coll).add(Integer.parseInt(vp.key), value);
        } else if ( vp.coll instanceof JsonObject ) {
            ((JsonObject)vp.coll).put(vp.key, value);
        }
        return target;
    }

    @Override
    public <T extends JsonStructure> T remove(T target) {
        JsonColKey vp = step(target);
        if ( vp.coll instanceof JsonArray ) {
            ((JsonArray)vp.coll).remove(Integer.parseInt(vp.key));
        } else if ( vp.coll instanceof JsonObject ) {
            ((JsonObject)vp.coll).remove(vp.key);
        }
        return target;
    }

    @Override
    public <T extends JsonStructure> T replace(T target, JsonValue value) {
        JsonColKey vp = step(target);
        if ( vp.coll instanceof JsonArray ) {
            ((JsonArray)vp.coll).remove(Integer.parseInt(vp.key));
            ((JsonArray)vp.coll).add(Integer.parseInt(vp.key), value);
        } else if ( vp.coll instanceof JsonObject ) {
            ((JsonObject)vp.coll).remove(vp.key);
            ((JsonObject)vp.coll).put(vp.key, value);
        }
        return target;
    }

    @Override
    public boolean containsValue(JsonStructure target) {
        JsonColKey vp = step(target);
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public JsonValue getValue(JsonStructure target) {
        JsonColKey vp = step(target);
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    static class ValuePointer {
        final JSONValue val;
        final JSONPointer path;
        
        ValuePointer(JSONValue value, JSONPointer p) {
            val = value;
            path = p;
        }
    }

    static class Value2Pointer {
        final JSON2Value val;
        final JSONPointer path;
        
        Value2Pointer(JSON2Value value, JSONPointer p) {
            val = value;
            path = p;
        }
    }

    public static class JsonValuePointer {
        public final JsonValue val;
        public final JSONPointer path;
        
        JsonValuePointer(JsonValue value, JSONPointer p) {
            val = value;
            path = p;
        }
    }
    
    /**
     * JSONPatch用
     * @param src
     * @param keep 1段前までで止める
     * @return 
     */
    public ValuePointer step(JSONValue src, boolean keep) {
        String[] ds = toDecodeString();
        JSONValue tg = src;
        if (ds.length == 1) {
            return new ValuePointer(tg, null);
        } else if (ds.length == 2 && keep) {
            return new ValuePointer(tg, this);
        } else if (tg instanceof JSONCollection) {
            tg = ((JSONCollection) tg).getJSON(ds[1]);
            return sub().step(tg, keep);
        } else {
            throw new java.lang.UnsupportedOperationException();
        }
    }
    
    /**
     * JSONPatch用
     * @param src
     * @param keep 1段前までで止める
     * @return 
     */
    public Value2Pointer step(JSON2Value src, boolean keep) {
        String[] ds = toDecodeString();
        JSON2Value tg = src;
        if (ds.length == 1) {
            return new Value2Pointer(tg, null);
        } else if (ds.length == 2 && keep) {
            return new Value2Pointer(tg, this);
        } else if (tg instanceof JSON2Collection) {
            tg = ((JSON2Collection) tg).getJSON(ds[1]);
            return sub().step(tg, keep);
        } else {
            throw new java.lang.UnsupportedOperationException();
        }
    }

    /**
     * JSON版
     * @param obj
     * @return 
     */
    private ColKey step(JSONCollection obj) {
        JSONPointer.ValuePointer vp = step((JSONValue)obj, true);
        ColKey kv = new ColKey();
        kv.coll = (JSONCollection) vp.val;
        kv.key = vp.path.toDecodeString()[1];
        return kv;
    }
    
    /**
     * JSON版
     * @param obj
     * @return 
     */
    private Col2Key step(JSON2Collection obj) {
        JSONPointer.Value2Pointer vp = step((JSON2Value)obj, true);
        Col2Key kv = new Col2Key();
        kv.coll = (JSON2Collection) vp.val;
        kv.key = vp.path.toDecodeString()[1];
        return kv;
    }

    public JsonValuePointer step(JsonValue src, boolean keep) {
        String[] ds = toDecodeString();
        JsonValue tg = src;
        if (ds.length == 1) {
            return new JsonValuePointer(tg, null);
        } else if (ds.length == 2 && keep) {
            return new JsonValuePointer(tg, this);
        } else if (tg instanceof JsonArray) {
            tg = ((JsonArray) tg).get(Integer.parseInt(ds[1]));
            return sub().step(tg, keep);
        } else if (tg instanceof JsonObject) {
            tg = ((JsonObject) tg).get(ds[1]);
            return sub().step(tg, keep);
        } else {
            throw new java.lang.UnsupportedOperationException();
        }
    }
    
    /**
     * Json版
     * @param obj Json
     * @return 
     */
    private JsonColKey step(JsonStructure obj) {
        JSONPointer.JsonValuePointer vp = step(obj, true);
        JsonColKey kv = new JsonColKey();
        kv.coll = (JsonStructure) vp.val;
        return kv;
    }
}
