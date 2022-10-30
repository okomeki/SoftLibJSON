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

import java.math.BigDecimal;
import java.math.BigInteger;
import net.siisise.json.JSONNumber;
import net.siisise.json.bind.MtoConvert;
import net.siisise.json.bind.OMConvert;

/**
 *
 */
public class JSONNumberM implements OMConvert {

    @Override
    public Class[] getSrcClasses() {
        return new Class[]{javax.json.JsonNumber.class, javax.json.JsonNumber.class, JSONNumber.class, Number.class, Integer.class, Long.class, Float.class, Short.class, BigInteger.class, Double.class, BigDecimal.class};
    }

    @Override
    public Object valueOf(Object obj, MtoConvert target) {
        if (obj instanceof javax.json.JsonNumber) { // JSONNumber , JSON2Number も該当
            obj = ((javax.json.JsonNumber) obj).numberValue();
        } else if (obj instanceof javax.json.JsonNumber) {
            obj = ((javax.json.JsonNumber) obj).numberValue();
        }
        if (obj instanceof Number) {
            return target.numberValue((Number) obj);
        }
        return this;
    }
}
