/*
 * Copyright 2023 okome.
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
package net.siisise.json;

import java.util.stream.Collector;
import net.siisise.bind.Rebind;
import net.siisise.io.FrontPacket;
import net.siisise.json.base.JSONBaseNULL;
import net.siisise.json.bind.target.JSONFormat;
import net.siisise.json.parser.JSONCReg;

/**
 * Microsoft 拡張のJSONC のコメント追加だけしたもの.
 * Java / C っぽいコメントが可能。
 */
interface JSONC extends JSON {

    /**
     * JSON文字列からObjectにパースする.
     * Number, String, Boolean, List, Map, null なんかで返る。
     * @param json json stream
     * @return Number, String, Boolean, List, Map, null などかな
     */
    static Object parse(String json) {
        return JSONCReg.parse(json);
    }

    /**
     * JSONっぽくくるんで返す。
     * 
     * 中身はJavaっぽくなっているのかも。
     * @param json
     * @return JSON2Valueな値
     */
    public static JSONValue parseWrap(String json) {
        return valueWrap(JSONCReg.parse(json));
    }

    /**
     * JSONデータ列からObjectにパースする
     *
     * @param json json stream
     * @return Java Objects
     */
    static Object parse(byte[] json) {
        return JSONCReg.parse(json);
    }

    /**
     * JSONっぽくくるんで返す。
     * 中身はJavaっぽくなっているのかも。
     * @param json json stream
     * @return JSON2Valueな値
     */
    public static JSONValue parseWrap(byte[] json) {
        return valueWrap(JSONCReg.parse(json));
    }

    /**
     * JSONデータ列からObjectにパースする
     *
     * @param json json stream
     * @return java objects
     */
    static Object parse(FrontPacket json) {
        return JSONCReg.parse(json);
    }

    /**
     * JSONっぽくくるんで返す。
     * 中身はJavaっぽくなっているのかも。
     * @param json json stream
     * @return JSON2Valueな値
     */
    public static JSONValue parseWrap(FrontPacket json) {
        return valueWrap(JSONCReg.parse(json));
    }

    public static final JSONFormat NOBR = new JSONFormat("","", true);
    public static final JSONFormat TAB = new JSONFormat("\r\n","  ", true);
    public static final JSONFormat NOBR_MINESC = new JSONFormat("","", false);
    public static final JSONFormat TAB_MINESC = new JSONFormat("\r\n","  ", false);

    /**
     * JSON中間型(風) Listまたは Map型で返す。
     *
     * @param src
     * @return JSON2系ListとMapのJavaっぽいデータ
     */
    public static Object valueMap(Object src) {
        return Rebind.valueOf(src, Object.class);
    }

    /**
     * valueOf で結果が単体(primitive型)のときラップしてから返す
     * srcがJSON2Valueのとき透過がいい.
     * JSONB では toJson に相当するかもしれない.
     *
     * @param src
     * @return JSON2Valueな値
     */
    public static JSONValue valueOf(Object src) {
        return Rebind.valueOf(src, JSONValue.class);
    }

    /**
     * JSONとして処理できる皮をかぶせる.
     * @param val
     * @return 
     */
    static JSONValue valueWrap(Object val) {
        if (val == null) {
            return JSONBaseNULL.NULL;
        } else if (val instanceof JSONValue) {
            return (JSONValue) val;
        } else if (val instanceof Boolean) {
            return (Boolean) val ? JSONBoolean.TRUE : JSONBoolean.FALSE;
        } else if (val instanceof Number) {
            return new JSONNumber((Number) val);
        } else if (val instanceof CharSequence) {
            return new JSONString((CharSequence) val);
        }
        throw new UnsupportedOperationException("未" + val.getClass().getName());
    }

    /**
     * 要素をそのまま維持してJSON2Arrayにする。
     *
     * @param <T>
     * @return
     */
    public static <T> Collector<T, ?, JSONArray> toJSON2Array() {
        return Collector.of(
                JSONArray::new,
                JSONArray::add,
                (ls, vals) -> {
                    ls.addAll(vals);
                    return ls;
                },
                Collector.Characteristics.IDENTITY_FINISH
        );
    }

    /**
     * StreamからREST寄りのprimitive型に変換して格納する。
     *
     * @param <T>
     * @return
     */
    public static <T> Collector<T, ?, JSONArray> toJSON2PrimArray() {
        return Collector.of(
                JSONArray::new,
                JSONArray::addValue,
                (ls, vals) -> {
                    vals.forEach(ls::addValue);
                    return ls;
                },
                Collector.Characteristics.IDENTITY_FINISH
        );
    }
    
    /**
     * Object と Arrayだけ複製が必要なのでそうする
     * @param v 元
     * @return 複製
     */
    public static JSONValue copy(JSONValue v) {
        if ( v instanceof JSONArray ) {
            return ((JSONArray) v).clone();
        } else if ( v instanceof JSONObject ) {
            return ((JSONObject) v).clone();
        }
        return v;
    }
}
