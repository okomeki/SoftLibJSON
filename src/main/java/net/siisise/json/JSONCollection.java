package net.siisise.json;

import java.util.Set;
import net.siisise.json.pointer.JSONPointer;

/**
 * JavaのCollection系classと互換性のありそうなJSON要素。
 * 内部に要素を持つArrayとObjectが該当。
 * ListとMap、Beanっぽいclassに自動マッピング可能
 * Mapのkey と Arrayのindexを統合してみたもの
 *
 * @param <T> 全体
 */
abstract public class JSONCollection<T> extends JSONValue<T> implements Iterable<JSONValue> {

    /**
     * JSON Patchで使う.
     *
     * @param key Map継承できるようObject
     * @return
     */
    abstract public JSONValue get(Object key);

    /**
     * JSON Patch用Array寄りの名称。
     * put/setは同じ
     *
     * @param key
     * @param obj
     */
    abstract public void set(String key, Object obj);

    /**
     * JSONPatch用追加。
     *
     * @param key
     * @param obj
     */
    abstract public void add(String key, Object obj);

    /**
     * Map寄りのメソッド(Arrayはsetと同)
     *
     * @param key
     * @param obj
     */
    abstract public void put(String key, Object obj);

    /**
     * 要素の削除。
     * 曖昧 ToDo: Listのremove(int index) と Collectionのremove(Object o)
     * に分ける?
     *
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
     * 何にでも変換できるといい。
     * JSONArrayからList<String> にしたい場合 List.class, String.class で変換される。
     * JSONObjectからMap<String,Example> の場合 Map.class, String.class, Example.class と指定する。
     * Collection以外(class,array)の内部要素は変数型から判定するので別途指定は不要。
     * 省略時、不足する場合はJSONValueから変換しない。
     *
     * @param <T>
     * @param clss
     * @return
     */
    abstract <T> T map(Class... clss);

    public JSONValue get(JSONPointer path) {
        JSONPointer.ValuePointer vp = path.step(this, false);
        return vp.val;
    }

    public void set(JSONPointer path, Object val) {
        ColKey vp = step(path);
        vp.coll.set(vp.key, val);
    }

    /**
     * JSONPatch の追加機能 Array / Object共通
     *
     * @param path
     * @param val
     */
    public void add(JSONPointer path, Object val) {
        ColKey vp = step(path);
        vp.coll.add(vp.key, val);
    }

    public void remove(JSONPointer path) {
        ColKey vp = step(path);
        vp.coll.remove(vp.key);
    }

    public void replace(JSONPointer path, Object val) {
        ColKey vp = step(path);
        vp.coll.remove(vp.key);
        vp.coll.add(vp.key, val);
    }
    
    private static class ColKey {
        private JSONCollection coll;
        private String key;
        
    }
    
    private ColKey step(JSONPointer path) {
        JSONPointer.ValuePointer vp = path.step(this, true);
        ColKey kv = new ColKey();
        kv.coll = (JSONCollection) vp.val;
        kv.key = vp.path.toDecodeString()[1];
        return kv;
    }
}
