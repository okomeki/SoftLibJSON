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

import java.math.BigDecimal;
import java.math.BigInteger;
import net.siisise.bind.format.TypeFormat;
import net.siisise.json.JSONValue;

/**
 * 
 * 両方対応する元.
 * 
 */
public class JSONBaseNumber extends Number implements JSONValue {

    private static final long serialVersionUID = 1L;
    
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

    public Number numberValue() {
        return number;
    }

    public Number get() {
        return number;
    }

    @Override
    public <T> T map() {
        return (T)number;
    }

    @Override
    public String toString() {
        return number.toString();
    }
    
    @Override
    public <V> V rebind(TypeFormat<V> format) {
        return format.numberFormat(number);
    }

    /**
     * 整数か?
     * @return 
     */
    public boolean isIntegral() {
        if (number instanceof Integer || number instanceof Long || number instanceof Short || number instanceof Byte || number instanceof BigInteger) {
            return true;
        }
        return bigDecimalValue().scale() >= 0;
    }

    public int intValueExact() {
        return bigDecimalValue().intValueExact();
    }

    public long longValueExact() {
        return bigDecimalValue().longValueExact();
    }

    public BigInteger bigIntegerValue() {
        return typeMap(BigInteger.class);
    }
    
    public BigInteger bigIntegerValueExact() {
        return bigDecimalValue().toBigIntegerExact();
    }

    public BigDecimal bigDecimalValue() {
        return typeMap(BigDecimal.class);
    }

}
