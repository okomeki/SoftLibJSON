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
    private final JSON2Value patch;
    
    public JSONMergePatch7396(JSON2Value patch) {
        this.patch = patch;
    }
    
    /**
     * おりじなるとぱっちから更新結果を返す
     * @param target Object 相当
     * @param patch
     * @return result
     */
    public static JSON2Value mergePatch(JSON2Value target, JSON2Value patch) {
        if ( patch instanceof JSON2Object ) {
            if ( target == null || !(target instanceof JSON2Object) ) {
                target = new JSON2Object();
            }
            for ( String name : ((JSON2Object<?>)patch).keySet() ) {
                JSON2Value v = ((JSON2Object) patch).getJSON(name);
                if ( v instanceof JSON2NULL ) {
                    if ( ((JSON2Object<?>)target).get(name) != null ) {
                        ((JSON2Object<?>)target).remove(name);
                    }
                } else {
                    ((JSON2Object<?>)target).addJSON(name, mergePatch(((JSON2Object)target).getJSON(name), v));
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
    public static JSON2Value diff(JSON2Value original, JSON2Value target) {
        JSON2Object diff = new JSON2Object();
        if ( (original instanceof JSON2Object) && (target instanceof JSON2Object)) {
            Set<String> skeyset = new HashSet<>(((JSON2Object)original).keySet());
            Set<String> tkeyset = ((JSON2Object) target).keySet();
            for ( String tkey : tkeyset ) {
                JSON2Value s = ((JSON2Object)original).getJSON(tkey);
                JSON2Value t = ((JSON2Object)target).getJSON(tkey);
                if ( s == null ) {
                    // 追加
                    diff.putJSON(tkey, JSON2.copy(t));
                } else if ( !s.equals(t)) {
                    // object なら差分を作る
                    JSON2Value d = diff(s, t);
                    diff.putJSON(tkey, d);
                }
                skeyset.remove(tkey);
            }
            for ( String skey : skeyset ) {
                diff.putJSON(skey, JSON2NULL.NULL);
            }
            return diff;
        } else { // targetがObjectではなかった
            return JSON2.copy(target);
        }
    }

    @Override
    public JsonValue apply(JsonValue target) {
        return mergePatch(JSON2.valueOf(target), patch).toJson();
    }

    @Override
    public JsonValue toJsonValue() {
        return patch.toJson();
    }
    
    public JSON2Value toJSON() {
        return JSON2.copy(patch);
    }
}
