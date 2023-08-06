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
import net.siisise.bind.format.TypeFormat;
import net.siisise.json.JSONNumber;

/**
 * JSON Number を Number にする変換.
 */
public class UnbindJSONNumber implements TypeUnbind {

    @Override
    public Type[] getSrcTypes() {
        return new Type[]{javax.json.JsonNumber.class, JSONNumber.class};
    }

    @Override
    public Object valueOf(Object obj, TypeFormat target) {
        if (obj instanceof javax.json.JsonNumber) { // JSONNumber , JSON2Number も該当
            obj = ((javax.json.JsonNumber) obj).numberValue();
        }
        if (obj instanceof Number) {
            return target.numberFormat((Number) obj);
        }
        return this;
    }
}
