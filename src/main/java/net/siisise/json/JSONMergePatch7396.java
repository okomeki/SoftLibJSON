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
package net.siisise.json;

import java.util.HashSet;
import java.util.Set;
import javax.json.JsonMergePatch;
import javax.json.JsonValue;
import net.siisise.json.base.JSONBaseNULL;
import net.siisise.json.bind.OMAP;

/**
 * RFC 7396 JSON Merge Patch.
 * 
 * JSON Object の差分を当てる JSON Merge Patch
 * とそれを作るdiff
 * 
 * RFC 7159 JSON
 * RFC 5789 PATCH Method for HTTP
 * RFC 7396 JSON Merge Patch
 * https://tools.ietf.org/html/rfc7396
 */
public class JSONMergePatch7396 implements JsonMergePatch {
    
    // 4.IANA Considerations
    public static final String typeName = "application"; 
    public static final String subtypeName = "merge-patch+json";
    private final JSONValue patch;
    
    public JSONMergePatch7396(JSONValue patch) {
        this.patch = patch;
    }
    
    /**
     * おりじなるとぱっちから更新結果を返す
     * @param target Object 相当
     * @param patch
     * @return result
     */
    public static JSONValue mergePatch(JSONValue target, JSONValue patch) {
        if ( patch instanceof JSONObject ) {
            if ( target == null || !(target instanceof JSONObject) ) {
                target = new JSONObject();
            }
            for ( String name : ((JSONObject<?>)patch).keySet() ) {
                JSONValue v = ((JSONObject) patch).getJSON(name);
                if ( v instanceof JSONBaseNULL ) {
                    if ( ((JSONObject<?>)target).get(name) != null ) {
                        ((JSONObject<?>)target).remove(name);
                    }
                } else {
                    ((JSONObject<?>)target).addJSON(name, mergePatch(((JSONObject)target).getJSON(name), v));
                }
            }
            return target;
        } else { // array string number boolean null
            return patch;
        }
    }
    
    /**
     * JSONMergePatchをつくろう
     * 差分の自動生成がなかったので作ってみるか
     * @param original 変更もと
     * @param target 変更先
     * @return JSON MergePatch形式 差分
     */
    public static JSONValue diff(JSONValue original, JSONValue target) {
        JSONObject diff = new JSONObject();
        if ( (original instanceof JSONObject) && (target instanceof JSONObject)) {
            Set<String> skeyset = new HashSet<>(((JSONObject)original).keySet());
            Set<String> tkeyset = ((JSONObject) target).keySet();
            for ( String tkey : tkeyset ) {
                JSONValue s = ((JSONObject)original).getJSON(tkey);
                JSONValue t = ((JSONObject)target).getJSON(tkey);
                if ( s == null ) {
                    // 追加
                    diff.putJSON(tkey, JSON.copy(t));
                } else if ( !s.equals(t)) {
                    // object なら差分を作る
                    JSONValue d = diff(s, t);
                    diff.putJSON(tkey, d);
                }
                skeyset.remove(tkey);
            }
            for ( String skey : skeyset ) {
                diff.putJSON(skey, JSONBaseNULL.NULL);
            }
            return diff;
        } else { // targetがObjectではなかった
            return JSON.copy(target);
        }
    }

    @Override
    public JsonValue apply(JsonValue target) {
        return OMAP.valueOf( mergePatch(JSON.valueOf(target), patch), JsonValue.class);
    }

    @Override
    public JsonValue toJsonValue() {
        return OMAP.valueOf(patch, JsonValue.class);
    }
    
    public JSONValue toJSON() {
        return JSON.copy(patch);
    }
}
