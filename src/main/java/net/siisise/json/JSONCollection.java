package net.siisise.json;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import net.siisise.json.pointer.JSONPointer;

/**
 * JavaのCollection系classと互換性のありそうなJSON要素
 * 内部に要素を持つArrayとObjectが該当。
 * ListとMap、Beanっぽいclassに自動マッピング可能
 * Mapのkey と Arrayのindexを統合してみたもの
 * @param <T> 全体
 */
abstract public class JSONCollection<T> extends JSONValue<T> implements Iterable<JSONValue> {

    /**
     * JSON Patchで使う
     * @param key Map継承できるようObject
     * @return 
     */
    abstract public JSONValue get(Object key);

    /**
     * JSON Patch用Array寄りの名称
     * put/setは同じ
     * @param key
     * @param obj 
     */
    abstract public void set(String key, Object obj);

    /**
     * JSONPatch用
     * @param key
     * @param obj 
     */
    abstract public void add(String key, Object obj);

    /**
     * Map寄りのメソッド(Arrayはsetと同)
     * @param key
     * @param obj 
     */
    abstract public void put(String key, Object obj);

    /**
     * ToDo: Listのremove(int index) と Collectionのremove(Object o) に分ける?
     * @param key
     * @return nullのときは該当なし?
     */
    abstract public JSONValue remove(Object key);
    
    // Collection系の機能
    abstract public int size();
    abstract public void clear();
    abstract public boolean isEmpty();
    abstract public Set<String> keySet();
    
    /**
     * プリミティブを含む配列、Collection、などに変換する。
     * 何にでも変換できるといい
     * JSONArrayからList<String> にしたい場合 List.class, String.class で変換される。
     * JSONObjectからMap<String,Example> の場合 Map.class, String.class, Example.class と指定する。
     * Collection以外(class,array)の内部要素は変数型から判定するので別途指定は不要。
     * 省略時、不足する場合はJSONValueから変換しない。
     * @param <T>
     * @param clss
     * @return 
     */
    abstract <T> T map(Class... clss);

    private static class VR {

        JSONValue val;
        JSONPointer path;

        VR(JSONValue v, JSONPointer p) {
            val = v;
            path = p;
        }
    }

    public JSONValue get(JSONPointer path) {
        VR vp = step(this, path, false);
        return vp.val;
    }

    public void set(JSONPointer path, Object val) {
        VR vp = step(this, path, true);
        String k = vp.path.toDecodeString()[1];
        ((JSONCollection) vp.val).set(k, val);
    }

    /**
     * JSONPatch の追加機能 Array / Object共通
     * @param path
     * @param val 
     */
    public void add(JSONPointer path, Object val) {
        VR vp = step(this, path, true);
        String k = vp.path.toDecodeString()[1];
        ((JSONCollection) vp.val).add(k, val);
    }

    public void remove(JSONPointer path) {
        VR vp = step(this, path, true);
        String k = vp.path.toDecodeString()[1];
        ((JSONCollection) vp.val).remove(k);
    }

    public void replace(JSONPointer path, Object val) {
        VR vp = step(this, path, true);
        String k = vp.path.toDecodeString()[1];
        ((JSONCollection) vp.val).remove(k);
        ((JSONCollection) vp.val).add(k, val);
    }

    private static VR step(JSONValue src, JSONPointer jp, boolean keep) {
        String[] ds = jp.toDecodeString();
        JSONValue tg = src;
        if (ds.length == 1) {
            return new VR(tg, null);
        } else if (ds.length == 2 && keep) {
            return new VR(tg, jp);
        } else if (tg instanceof JSONCollection) {
            tg = ((JSONCollection) tg).get(ds[1]);
//            if ( ds.length == 2 ) {
//                return new VR(tg,null);
//            } else {
            return step(tg, jp.sub(), keep);
//            }
        } else {
            throw new java.lang.UnsupportedOperationException();
        }
    }


    public static JSONArray convList(Collection list) {
        List<JSONValue> val = new ArrayList<>();
        for (Object src : list) {
            val.add(JSONValue.valueOf(src));
        }
        return new JSONArray(val);
    }
}
