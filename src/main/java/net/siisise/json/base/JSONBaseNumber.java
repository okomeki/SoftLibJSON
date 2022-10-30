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
package net.siisise.json.base;

import java.lang.reflect.Type;
import java.math.BigDecimal;
import java.math.BigInteger;
import net.siisise.json.JSONFormat;
import net.siisise.json.JSONValue;
import net.siisise.json.bind.OMAP;

/**
 * 両方対応する元
 */
public class JSONBaseNumber extends Number implements JSONValue {
    
    protected final Number number;
    
    protected JSONBaseNumber(Number num) {
        number = num;
    }
    
    @Override
    public int intValue() {
        return number.intValue();
    }

    @Override
    public long longValue() {
        return number.longValue();
    }

    @Override
    public float floatValue() {
        return number.floatValue();
    }

    @Override
    public double doubleValue() {
        return number.doubleValue();
    }

    @Override
    public <T> T typeMap(Type type) {
        return (T) OMAP.typeNumber(number, type);
    }

    public Number numberValue() {
        return number;
    }

    @Override
    public String toString() {
        return number.toString();
    }
    
    @Override
    public javax.json.JsonNumber toJson() {
        return OMAP.typeNumber(number, javax.json.JsonNumber.class);
    }

    @Override
    public javax.json.JsonNumber toXJson() {
        return OMAP.typeNumber(number, javax.json.JsonNumber.class);
    }
    
    @Override
    public String toJSON() {
        return number.toString();
    }

    @Override
    public String toJSON(JSONFormat format) {
        return toJSON();
    }

    @Override
    public <T> T map() {
        return (T)number;
    }

    public boolean isIntegral() {
        if (number instanceof Integer || number instanceof Long || number instanceof Short || number instanceof Byte || number instanceof BigInteger) {
            return true;
        }
        if (number instanceof Float || number instanceof Double || number instanceof BigDecimal) {
            return false;
        }
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    public int intValueExact() {
        return ((BigDecimal)typeMap(BigDecimal.class)).intValueExact();
    }

    public long longValueExact() {
        return ((BigDecimal)typeMap(BigDecimal.class)).longValueExact();
    }

    public BigInteger bigIntegerValue() {
        return typeMap(BigInteger.class);
    }
    
    public BigInteger bigIntegerValueExact() {
        return ((BigDecimal)typeMap(BigDecimal.class)).toBigIntegerExact();
    }

    public BigDecimal bigDecimalValue() {
        return typeMap(BigDecimal.class);
    }

}
