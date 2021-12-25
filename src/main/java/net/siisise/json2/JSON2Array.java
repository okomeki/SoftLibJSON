package net.siisise.json2;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import javax.json.JsonArray;
import javax.json.JsonValue;
import net.siisise.json2.jsonp.JSONPArray;
import net.siisise.omap.OMAP;

/**
 * Listを拡張したJSONArray。
 * 一般のデータを保持してJSONにも変換可能なスタイル。
 * 配列、Listの他ObjectのコンストラクタにもtypeMap可能。
 * JSONP準拠のものはEをJsonValueにするといい。
 * 
 * JsonArray,JsonArrayBuilder,JsonStructure ではない
 * @param <E> 内部で保持する型。JSONではなくていい。
 */
public class JSON2Array<E> extends ArrayList<E> implements JSON2Collection<E> {

    /**
     * Eを参照したいので持っておく型
     * JsonArray用かもしれない
     */
    private final Class<E> def;

    public JSON2Array() {
        def = null;
    }

    protected JSON2Array(Class<E> c) {
        def = c;
    }

    public JSON2Array(Collection<E> vals) {
        super(vals);
        def = null;
    }

    @Override
    public JSON2Value getJSON(String key) {
        return JSON2.valueOf(get(Integer.parseInt(key)));
    }

//    JSON2Value getJSON(int index) {
//        return JSON2.valueOf(get(index));
//    }

    @Override
    public void setJSON(String key, JSON2Value obj) {
        E val = def == null ? obj.map() : obj.typeMap(def);
        if (key.equals("-")) {
            add(val);
        } else {
            set(Integer.parseInt(key), val);
        }
    }

    @Override
    public void addJSON(String key, JSON2Value obj) {
        E val = def == null ? obj.map() : obj.typeMap(def);
        if (key.equals("-")) {
            add(val);
        } else {
            add(Integer.parseInt(key), val);
        }
    }

    /**
     * 指定位置のデータを配列から取り除く
     * @param key 配列のindex相当文字列
     * @return 削除した値
     */
    @Override
    public JSON2Value removeJSON(String key) {
        return JSON2.valueOf(remove(Integer.parseInt(key)));
    }

    @Override
    public JSON2Value putJSON(String key, JSON2Value obj) {
        JSON2Value val = getJSON(key);
        setJSON(key, obj);
        return val;
    }

    /**
     * primitive に変える
     *
     * @param val
     */
    public void addValue(Object val) {
        if (def != null) {
            val = OMAP.valueOf(val,def);
        } else {
            val = JSON2.valueMap(val); // Object系に変換
        }
        add((E) val);
    }

    @Override
    public <T> T typeMap(Type type) {
        return OMAP.typeList(this, type);
    }

    Stream<JSON2Value> j2Stream() {
        return parallelStream().map(JSON2::valueOf);
    }
    
    /**
     * データを適当な型に変換して納めるので変換可能な配列ならなんでもいい。
     * @param <T>
     * @param a 抽出したい型の配列 0または必要数
     * @return
     */
    @Override
    public <T> T[] toArray(T[] a) {
        Class contentType = a.getClass().getComponentType();
        if (def != null && contentType == def) {
            return (T[]) toArray();
        }
        return j2Stream().map(v -> v.typeMap(contentType)).collect(Collectors.toList()).toArray(a);
    }

    /**
     * JSON2な
     * @return JsonArrayに 
     */
    @Override
    public JsonArray toJson() {
        if (isEmpty()) {
            return JsonValue.EMPTY_JSON_ARRAY;
        }
        JSONPArray ar = new JSONPArray();
        j2Stream().map(v -> v.toJson()).forEach(ar::add);
        return ar;
    }

    @Override
    public String toString() {
        return toString(NOBR);
    }

    @Override
    public String toString(JSON2Format format) {
        return j2Stream().map(val -> 
            format.crlf + format.tab + tab(val.toString(format)))
                .collect( Collectors.joining(",", "[", format.crlf +  "]"));
    }

    /**
     * ListっぽいJSON2Arrayの複製(表面のみ)で返す。
     * @param <T>
     * @return 
     */
    @Override
    public <T> T map() {
        return (T)new JSON2Array(this);
    }
}
