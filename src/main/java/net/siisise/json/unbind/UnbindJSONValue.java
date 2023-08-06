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
package net.siisise.json.unbind;

import java.lang.reflect.Type;
import net.siisise.bind.TypeUnbind;
import net.siisise.bind.format.TypeBind;
import net.siisise.bind.format.TypeFormat;
import net.siisise.json.JSONBoolean;
import net.siisise.json.base.JSONBaseNULL;

/**
 * Boolean, NULL, JSONValue
 */
public class UnbindJSONValue implements TypeUnbind {

    @Override
    public Type[] getSrcTypes() {
        return new Type[]{javax.json.JsonValue.class, JSONBaseNULL.class, JSONBoolean.class};
    }

    @Override
    public Object valueOf(Object obj, TypeFormat format) {
        if ( obj == javax.json.JsonValue.NULL || obj instanceof JSONBaseNULL ) {
            return format.nullFormat();
        } else if (format instanceof TypeBind && ((TypeBind)format).targetClass() instanceof Class && ((Class) ((TypeBind)format).targetClass()).isInstance(obj)) { // 仮位置
            return obj;
        } else if (obj == javax.json.JsonValue.TRUE || obj == JSONBoolean.TRUE) {
            return format.booleanFormat(true);
        } else if (obj == javax.json.JsonValue.FALSE || obj == JSONBoolean.FALSE) {
            return format.booleanFormat(false);
        }

        return this;
    }

}
