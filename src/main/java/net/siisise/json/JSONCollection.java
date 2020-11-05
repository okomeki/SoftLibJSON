package net.siisise.json;

import java.lang.reflect.Type;
import java.util.Set;
import javax.json.JsonStructure;
import net.siisise.json.pointer.JSONPointer;

/**
 * JavaのCollection系classと互換性のありそうなJSON要素。
 * 内部に要素を持つArrayとObjectが該当。
 * ListとMap、Beanっぽいclassに自動マッピング可能
 * Mapのkey と Arrayのindexを統合してみたもの
 * List,Mapとかぶらないメソッド準備中。予定は未定。
 *
 * Java API for JSON ProcessingのJsonStructure ぐらいのもの、か?
 * @param <K>
 * @param <T> 全体
 */
public interface JSONCollection<K extends String,T> extends Iterable<JSONValue> {

    /**
     * JSON Patchで使う.
     *
     * @param key Map継承できるようObject
     * @return
     */
    JSONValue get(Object key);
    JSONValue get(K key);
    JSONValue getJSON(String key);
    JSONValue getJSON(JSONPointer point);
    
    /**
     * JSON Patch用Array寄りの名称。
     * put/setは戻り値以外同じ
     *
     * @param key arrayのindexまたはobjectのname
     * @param obj
     */
    void set(String key, Object obj);
    void setJSON(String key, JSONValue obj);

    /**
     * JSONPatch用追加。
     * 
     * @param key arrayのindexまたはobjectのname
     * @param obj
     */
    void add(String key, Object obj);
    void addJSON(String key, JSONValue obj);

    /**
     * Map寄りのメソッド(Arrayはsetと同)
     *
     * @param key arrayのindexまたはobjectのname
     * @param obj
     * @return 
     */
    Object put(String key, Object obj);
    JSONValue putJSON(String key, JSONValue obj);

    /**
     * 要素の削除。
     * 曖昧 ToDo: Listのremove(int index) と Collectionのremove(Object o)
     * に分ける?
     *
     * @param key
     * @return nullのときは該当なし?
     */
    JSONValue remove(Object key);
    JSONValue remove(String key);
    JSONValue removeJSON(String key);

    // Collection系の機能
    int size();
    void clear();
    boolean isEmpty();
    Set<K> keySet();
    Set<String> keySetJSON();

    /**
     * プリミティブを含む配列、Collection、などに変換する。
     * 何にでも変換できるといい。
     * JSONArrayからList<String> にしたい場合 List.class, String.class で変換される。
     * JSONObjectからMap<String,Example> の場合 Map.class, String.class, Example.class と指定する。
     * Collection以外(class,array)の内部要素は変数型から判定するので別途指定は不要。
     * 省略時、不足する場合はJSONValueから変換しない。
     * Typeを使用したtypeMap(Type) がおすすめかもしれない。
     * 
     * @deprecated #typeMap(Type) へ移行する
     * @param <T>
     * @param clss
     * @return
     */
    <T> T map(Class... clss);
    
    /**
     * オブジェクトマッピング JSON版.
     * JSONから指定された型のオブジェクトに変換する.
     * 何にでも変換できるといい。
     * プリミティブ型、Object、String、配列、Collection(List, Map, Set)、JSON for Java(JSONP) などを想定。
     * リフレクションを使用して、対応する型に自動変換できる想定。
     * String,Boolean,Numberと該当する型、JSONObjectとJava Object、JSONArrayとList,Set,配列の変換、
     * JSONArrayとJava コンストラクタの対応
     * その他拡張、などを想定してみたりみなかったり。
     * 
     * @param <T> 指定する型.
     * @param type 型情報. T相当の Class&lt;T&gt; またはType.
     * @return 変換された情報
     */
    <T> T typeMap(Type type);

    /**
     * JSONPに変換
     * @return 
     */
    JsonStructure toJson();

    // String key, JSONValue obj の変換の入らないもの MapやListとの重複を一応避ける

    /**
     * いろいろkey
     * @param key
     * @return 
     */
    
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
}