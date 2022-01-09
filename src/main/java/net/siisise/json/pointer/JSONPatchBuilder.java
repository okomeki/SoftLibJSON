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
package net.siisise.json.pointer;

import javax.json.JsonPatch;
import javax.json.JsonPatchBuilder;
import javax.json.JsonValue;
import net.siisise.json2.JSON2;
import net.siisise.json2.JSON2Array;

/**
 *
 */
public class JSONPatchBuilder implements JsonPatchBuilder {
    
    JSON2Array patchs;
    JSONPatch patch;
    
    public JSONPatchBuilder() {
        patchs = new JSON2Array();
        patch = new JSONPatch();
    }

    @Override
    public JsonPatchBuilder add(String path, JsonValue value) {
        JSONPatch.CmdAdd cmd = new JSONPatch.CmdAdd();
        cmd.path = new JSONPointer(path);
        cmd.value = JSON2.valueOf(value);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatchBuilder add(String path, String value) {
        JSONPatch.CmdAdd cmd = new JSONPatch.CmdAdd();
        cmd.path = new JSONPointer(path);
        cmd.value = JSON2.valueOf(value);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatchBuilder add(String path, int value) {
        JSONPatch.CmdAdd cmd = new JSONPatch.CmdAdd();
        cmd.path = new JSONPointer(path);
        cmd.value = JSON2.valueOf(value);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatchBuilder add(String path, boolean value) {
        JSONPatch.CmdAdd cmd = new JSONPatch.CmdAdd();
        cmd.path = new JSONPointer(path);
        cmd.value = JSON2.valueOf(value);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatchBuilder remove(String path) {
        JSONPatch.CmdRemove cmd = new JSONPatch.CmdRemove();
        cmd.path = new JSONPointer(path);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatchBuilder replace(String path, JsonValue value) {
        JSONPatch.CmdReplace cmd = new JSONPatch.CmdReplace();
        cmd.path = new JSONPointer(path);
        cmd.value = JSON2.valueOf(value);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatchBuilder replace(String path, String value) {
        JSONPatch.CmdReplace cmd = new JSONPatch.CmdReplace();
        cmd.path = new JSONPointer(path);
        cmd.value = JSON2.valueOf(value);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatchBuilder replace(String path, int value) {
        JSONPatch.CmdReplace cmd = new JSONPatch.CmdReplace();
        cmd.path = new JSONPointer(path);
        cmd.value = JSON2.valueOf(value);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatchBuilder replace(String path, boolean value) {
        JSONPatch.CmdReplace cmd = new JSONPatch.CmdReplace();
        cmd.path = new JSONPointer(path);
        cmd.value = JSON2.valueOf(value);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatchBuilder move(String path, String from) {
        JSONPatch.CmdMove cmd = new JSONPatch.CmdMove();
        cmd.path = new JSONPointer(path);
        cmd.from = new JSONPointer(from);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatchBuilder copy(String path, String from) {
        JSONPatch.CmdCopy cmd = new JSONPatch.CmdCopy();
        cmd.path = new JSONPointer(path);
        cmd.from = new JSONPointer(from);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatchBuilder test(String path, JsonValue value) {
        JSONPatch.CmdTest cmd = new JSONPatch.CmdTest();
        cmd.path = new JSONPointer(path);
        cmd.value = JSON2.valueOf(value);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatchBuilder test(String path, String value) {
        JSONPatch.CmdTest cmd = new JSONPatch.CmdTest();
        cmd.path = new JSONPointer(path);
        cmd.value = JSON2.valueOf(value);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatchBuilder test(String path, int value) {
        JSONPatch.CmdTest cmd = new JSONPatch.CmdTest();
        cmd.path = new JSONPointer(path);
        cmd.value = JSON2.valueOf(value);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatchBuilder test(String path, boolean value) {
        JSONPatch.CmdTest cmd = new JSONPatch.CmdTest();
        cmd.path = new JSONPointer(path);
        cmd.value = JSON2.valueOf(value);
        patch.cmds.add(cmd);
        return this;
    }

    @Override
    public JsonPatch build() {
        return patch;
    }
    
}
