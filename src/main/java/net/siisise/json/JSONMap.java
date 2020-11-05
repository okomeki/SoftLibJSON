package net.siisise.json;

import java.util.HashMap;
import java.util.Map;
import net.siisise.json.map.JSONArrayM;
import net.siisise.json.map.JSONDateM;
import net.siisise.json.map.JSONNumberM;
import net.siisise.json.map.JSONObjectM;
import net.siisise.json.map.JSONStringM;
import net.siisise.json.map.JSONUUIDM;
import net.siisise.json.map.JSONValueM;

/**
 * JSON 中間 Java の3形式を捌く.
 * J JSON String
 * M 中間形式
 * O Java Object
 * 
 * JSON2 中間を省略したい形式
 * J JSON String
 * M 中間形式 (表面のみ) List/Map/JSON2Number/JSON2String/JSON2Boolean/JSON2NULL 内部 List/Map/Number/Boolean/String/null
 * O List/Map/Number/Boolean/String/null / JavaObject,配列,Generic
 *
 * JM Parser
 * MJ toString
 * MO JSONReplaceMO
 * OM JSONReplcaeOM, toJSON
 *
 */
public class JSONMap {

    /**
     * 仮置き
     */
    static JSONReplaceMO[] CONVS = {
        new JSONDateM(),
        new JSONUUIDM()
    };

    /**
     * JSON中間形式からJavaオブジェクトに変換する
     */
    public static final Map<Class, JSONReplaceMO> replaces;

    static { // final用固定にする?
        replaces = new HashMap<>();
        for (JSONReplaceMO jr : CONVS) {
            replaces.put(jr.targetClass(), jr);
        }
    }

    JSONReplaceOM[] parsers = {
        new JSONValueM(),
        new JSONNumberM(),
        new JSONStringM(),
        new JSONArrayM(),
        new JSONObjectM()
    };

    public JSONMap() {
    }

    /**
     *
     * @param src
     * @return
     */
    public JSONValue valueOf(Object src) {
        return valueOf(src, null);
    }

    /**
     * なんでもJSONに変換する。 プリミティブ型、配列、Collection、Object boolean byte short char int
     * long float double List Map Number null String Date型など要検討
     *
     * @param src データ型なんでも
     * @param replacer
     * @return JSONValue
     */
    public JSONValue valueOf(Object src, JSONReplacer replacer) {
        for (JSONReplaceOM ps : parsers) {
            JSONValue val = (JSONValue) ps.valueOf(src, replacer);
            if (val != null) {
                return val;
            }
        }
        return JSONObject.convObject(src);
    }
}
