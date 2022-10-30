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
package net.siisise.json.bind.source;

import net.siisise.json.JSONBoolean;
import net.siisise.json.base.JSONBaseNULL;
import net.siisise.json.bind.MtoConvert;
import net.siisise.json.bind.OMConvert;

/**
 * Boolean, NULL, JSONValue
 */
public class JSONValueM implements OMConvert {

    @Override
    public Class[] getSrcClasses() {
        return new Class[]{javax.json.JsonValue.class, javax.json.JsonValue.class, JSONBaseNULL.class, JSONBoolean.class, Boolean.class};
    }

    @Override
    public Object valueOf(Object obj, MtoConvert outConvert) {
        if (obj == null || obj == javax.json.JsonValue.NULL || obj == javax.json.JsonValue.NULL || obj instanceof JSONBaseNULL ) {
            return outConvert.nullValue();
        } else if (outConvert.targetClass() instanceof Class && ((Class) outConvert.targetClass()).isInstance(obj)) { // 仮位置
            return obj;
        } else if (obj == javax.json.JsonValue.TRUE || obj == javax.json.JsonValue.TRUE || obj == JSONBoolean.TRUE) {
            return outConvert.booleanValue(true);
        } else if (obj == javax.json.JsonValue.FALSE || obj == javax.json.JsonValue.FALSE || obj == JSONBoolean.FALSE) {
            return outConvert.booleanValue(false);
        } else if (obj instanceof Boolean) {
            return outConvert.booleanValue((boolean) obj);
        }

        return this;
    }

}
