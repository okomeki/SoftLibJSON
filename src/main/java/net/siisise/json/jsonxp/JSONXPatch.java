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
package net.siisise.json.jsonxp;

import javax.json.JsonArray;
import javax.json.JsonPatch;
import javax.json.JsonStructure;
import net.siisise.json.JSON;
import net.siisise.json.JSONCollection;
import net.siisise.json.JSONValue;
import net.siisise.json.base.JSONBasePatch;

/**
 *
 */
public class JSONXPatch extends JSONBasePatch implements JsonPatch {

    @Override
    public <T extends JsonStructure> T apply(T target) {
        Class c = target.getClass();
        JSONCollection cp = (JSONCollection) JSON.valueOf(target);
        for (Patch cmd : cmds) {
            cp = cmd.apply(cp);
        }
        return cp.typeMap(c);
    }

    @Override
    public JsonArray toJsonArray() {
        return cmds.toXJson();
    }
    
    /**
     * add remove replace のみの差分を出力する move copy は未対応
     *
     * @param source
     * @param target
     * @return
     */
    public static JSONXPatch diff(JSONValue source, JSONValue target) {
        JSONXPatch patch = new JSONXPatch();
        JSONBasePatch.diff(source, target, patch);
        return patch;
    }
}
