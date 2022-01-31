/*
 * Copyright 2022 Siisise Net
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
package net.siisise.json;

import java.lang.reflect.Type;
import javax.json.JsonValue;
import net.siisise.json.bind.OMAP;

/**
 * JSON NULL.
 * 
 */
public class JSON2NULL implements JSON2Value,JsonValue {
    
    public static final JSON2NULL NULL = new JSON2NULL();

    @Override
    public <T> T map() {
        return null;
    }

    @Override
    public Object typeMap(Type type) {
        return OMAP.typeNull(type);
    }

    @Override
    public JsonValue toJson() {
        return JsonValue.NULL;
    }
    
    @Override
    public String toString() {
        return toJSON(NOBR);
    }
    
    public String toJSON() {
        return toJSON(NOBR);
    }

    @Override
    public String toJSON(JSON2Format format) {
        return "null";
    }
    
    @Override
    public ValueType getValueType() {
        return ValueType.NULL;
    }

    /**
     * JsonValueは一方的な比較
     * @param obj
     * @return 
     */
    public boolean equals(Object obj) {
        return (obj != null && (obj instanceof JSON2NULL || obj == JsonValue.NULL));
            
    }
    
}
