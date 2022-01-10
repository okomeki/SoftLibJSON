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
import net.siisise.io.FrontPacket;
import net.siisise.json2.JSON2;
import net.siisise.json2.JSON2Collection;
import net.siisise.json2.JSON2Value;

/**
 * RFC 6901 JSON Pointer
 *
 */
public class JSONPointer implements JsonPointer {

    private String[] path;

    public JSONPointer(String escapedPath) {
        if (!JSONPointerReg.jsonPointer.eq(escapedPath)) {
            throw new java.lang.UnsupportedOperationException();
        }
        this.path = escapedPath.split("/");
    }

    /**
     * 
     * @param lp reference-token の Packet列
     */
    JSONPointer(List<? extends FrontPacket> lp) {
        path = new String[lp.size() + 1];
        path[0] = "";
        int i = 1;
        for (FrontPacket p : lp) {
            path[i] = AbstractABNF.str(p);
            if (!JSONPointerReg.referenceToken.eq(path[i++])) {
                throw new java.lang.UnsupportedOperationException();
            }
        }
    }

    /**
     * wrap いらないかも
     *
     * @param target
     * @param value
     */
    public void add(JSON2Collection target, JSON2Value value) {
        ColKey<JSON2Collection> vp = step(target);
        vp.coll.addJSON(vp.key, value);
    }

    /**
     * 追加する JsonValueは不変らしいがどうするのが正解か
     * @param <T> targetの型
     * @param target
     * @param value
     * @return target の複製の方がいい? 
     */
    @Override
    public <T extends JsonStructure> T add(T target, JsonValue value) {
        ColKey<JsonStructure> vp = step(target);
        if (vp.coll instanceof JsonArray) {
            ((JsonArray) vp.coll).add(Integer.parseInt(vp.key), value);
        } else if (vp.coll instanceof JsonObject) {
            ((JsonObject) vp.coll).put(vp.key, value);
        }
        return target;
    }

    /**
     * 消した値を返す のでJSON Pointerとは別
     * @param target
     * @return 値?
     */
    public JSON2Value remove(JSON2Collection target) {
        ColKey<JSON2Collection> vp = step(target);
        return vp.coll.removeJSON(vp.key);
    }

    /**
     * JSON Pointerの動作に準拠
     * なにかちがう?
     *
     * @param <T>
     * @param target
     * @return 対象が削除された target 同じかもしれないし複製かもしれないし
     */
    @Override
    public <T extends JsonStructure> T remove(T target) {
        ColKey<JsonStructure> vp = step(target);
        if (vp.coll instanceof JsonArray) {
            ((JsonArray) vp.coll).remove(Integer.parseInt(vp.key));
        } else if (vp.coll instanceof JsonObject) {
            ((JsonObject) vp.coll).remove(vp.key);
        }
        return target;
    }

    /**
     * JsonPointerのgetValueがいいのかも
     * @param target
     * @return 
     */
    public JSON2Value get(JSON2Collection target) {
        return step((JSON2Value) target, false).val;
    }

    public void set(JSON2Collection target, Object value) {
        ColKey<JSON2Collection> vp = step(target);
        vp.coll.setJSON(vp.key, JSON2.valueOf(value));
    }

    public void replace(JSON2Collection target, Object value) {
        ColKey<JSON2Collection> vp = step(target);
        vp.coll.removeJSON(vp.key);
        vp.coll.addJSON(vp.key, JSON2.valueOf(value));
    }

    private static class ColKey<J> {

        private J coll;
        private String key;
    }

    public JSONPointer sub() {
        List<FrontPacket> lp = new ArrayList<>();
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
    
    public String toJSON() {
        return JSON2.valueOf(toString()).toString();
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

    /**
     * JSON Pointer escaped のデコード
     * @param str
     * @return
     * @throws MalformedInputException 
     */
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
    public <T extends JsonStructure> T replace(T target, JsonValue value) {
        ColKey<JsonStructure> vp = step(target);
        if (vp.coll instanceof JsonArray) {
            ((JsonArray) vp.coll).remove(Integer.parseInt(vp.key));
            ((JsonArray) vp.coll).add(Integer.parseInt(vp.key), value);
        } else if (vp.coll instanceof JsonObject) {
            ((JsonObject) vp.coll).remove(vp.key);
            ((JsonObject) vp.coll).put(vp.key, value);
        }
        return target;
    }

    @Override
    public boolean containsValue(JsonStructure target) {
        try {
            ColKey<JsonStructure> vp = step(target);
            return true;
        } catch (UnsupportedOperationException e) {
            return false;
        }
    }

    @Override
    public JsonValue getValue(JsonStructure target) {
        return step(target, false).val;
    }

    public static class ValuePointer<T> {

        public final T val;
        final JSONPointer path;

        ValuePointer(T value, JSONPointer p) {
            val = value;
            path = p;
        }
    }

    /**
     * JSONPatch用
     *
     * @param src
     * @param keep 1段前までで止める
     * @return
     */
    public <J> ValuePointer<J> step(J src, boolean keep) {
        String[] ds = toDecodeString();
        J tg = src;
        if (ds.length == 1) {
            return new ValuePointer(tg, null);
        } else if (ds.length == 2 && keep) {
            return new ValuePointer(tg, this);
        } else if (tg instanceof JSON2Collection) {
            tg = (J) ((JSON2Collection) tg).getJSON(ds[1]);
            return sub().step(tg, keep);
        } else {
            throw new java.lang.UnsupportedOperationException();
        }
    }

    /**
     * JSON版
     *
     * @param obj
     * @return
     */
    private ColKey<JSON2Collection> step(JSON2Collection obj) {
        JSONPointer.ValuePointer<JSON2Value> vp = step((JSON2Value) obj, true);
        ColKey<JSON2Collection> kv = new ColKey();
        kv.coll = (JSON2Collection) vp.val;
        kv.key = vp.path.toDecodeString()[1];
        return kv;
    }

    public ValuePointer<JsonValue> step(JsonValue src, boolean keep) {
        String[] ds = toDecodeString();
        JsonValue tg = src;
        if (ds.length == 1) {
            return new ValuePointer(tg, null);
        } else if (ds.length == 2 && keep) {
            return new ValuePointer(tg, this);
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
     *
     * @param obj Json
     * @return
     */
    private ColKey<JsonStructure> step(JsonStructure obj) {
        JSONPointer.ValuePointer<JsonValue> vp = step(obj, true);
        ColKey<JsonStructure> kv = new ColKey<>();
        kv.coll = (JsonStructure) vp.val;
        return kv;
    }
}
