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
package net.siisise.json.bind.target;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Map;
import javax.json.JsonValue;
import javax.json.spi.JsonProvider;
import net.siisise.bind.format.TypeBind;

/**
 * JsonpProvider を使ったら
 */
public class JsonpProviderConvert implements TypeBind<JsonValue> {

    private final JsonProvider provider;
    
    public JsonpProviderConvert(JsonProvider p) {
        provider = p;
    }
    
    public JsonpProviderConvert() {
        provider = JsonProvider.provider();
    }

    @Override
    public JsonValue nullFormat() {
        return JsonValue.NULL;
    }

    @Override
    public JsonValue booleanFormat(boolean bool) {
        return bool ? JsonValue.TRUE : JsonValue.FALSE;
    }

    @Override
    public JsonValue numberFormat(Number num) {
        if ( num instanceof Integer || num instanceof Short || num instanceof Byte ) {
            return provider.createValue(num.intValue());
        } else if ( num instanceof Long ) {
            return provider.createValue(num.longValue());
        } else if ( num instanceof Double ) {
            return provider.createValue(num.doubleValue());
        } else if ( num instanceof Float ) {
            return provider.createValue(num.floatValue());
        } else if ( num instanceof BigInteger ) {
            return provider.createValue((BigInteger)num);
        } else if ( num instanceof BigDecimal ) {
            return provider.createValue((BigDecimal)num);
        }
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public JsonValue stringFormat(String str) {
        return provider.createValue(str);
    }

    @Override
    public JsonValue mapFormat(Map map) {
        return provider.createObjectBuilder(map).build();
    }

    @Override
    public JsonValue collectionFormat(Collection col) {
        return provider.createArrayBuilder(col).build();
    }
}
