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
package net.siisise.json.unbind;

import net.siisise.bind.TypeUnbind;
import net.siisise.bind.UnbindList;
import net.siisise.json.map.JSONDateM;

/**
 *
 */
public class JSONUnbindList implements UnbindList {

    static final TypeUnbind[] JSONUNBINDS = {
        new UnbindJSONValue(),
        new UnbindJSONNumber(),
//        new UnbindUUID(),
        new JSONDateM(),
        new UnbindJSONString(),
        new UnbindToJSON()
    };

    @Override
    public TypeUnbind[] getList() {
        return JSONUNBINDS;
    }

    
    
}
