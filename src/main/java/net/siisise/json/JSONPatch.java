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

import java.util.List;
import javax.json.JsonArray;
import javax.json.JsonPatch;
import javax.json.JsonStructure;
import net.siisise.bind.Rebind;
import net.siisise.json.base.JSONBasePatch;

/**
 * RFC 6902 JavaScript Object Notation (JSON) Patch.
 * https://tools.ietf.org/html/rfc6902
 */
public class JSONPatch extends JSONBasePatch implements JsonPatch {

    public JSONPatch() {
    }

    /**
     * 
     * @param patchList JSON Patch
     */
    public JSONPatch(List patchList) {
        super(patchList);
    }

    /**
     * エラー未実装
     *
     * @param <T> 型変換対策
     * @param target 元は可変
     * @return target処理後の複製? エラーっぽいときはnull
     */
    public <T extends JSONCollection> T apply(T target) {
        target = (T) target.clone();
        for (Patch cmd : cmds) {
            target = cmd.apply(target);
        }
        return target;
    }

    @Override
    public <T extends JsonStructure> T apply(T target) {
        Class c = target.getClass();
        JSONCollection cp = (JSONCollection) JSON.valueOf(target);
        for (Patch cmd : cmds) {
            cp = cmd.apply(cp);
        }
        return Rebind.valueOf(cp, c);
    }

    @Override
    public JsonArray toJsonArray() {
        return Rebind.valueOf(cmds, JsonArray.class);
    }

    public static JSONPatch diff(JSONValue source, JSONValue target) {
        JSONPatch patch = new JSONPatch();
        JSONBasePatch.diff(source, target, patch);
        return patch;
    }
}
