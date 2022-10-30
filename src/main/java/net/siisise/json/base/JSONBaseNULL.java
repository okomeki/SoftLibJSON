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
import net.siisise.json.JSONFormat;
import net.siisise.json.JSONValue;
import net.siisise.json.bind.OMAP;

/**
 * 両方対応する元
 */
public class JSONBaseNULL implements JSONValue {
    
    public static final JSONBaseNULL NULL = new JSONBaseNULL();

    @Override
    public <T> T map() {
        return null;
    }

    @Override
    public Object typeMap(Type type) {
        return OMAP.typeNull(type);
    }

    @Override
    public javax.json.JsonValue toJson() {
        return javax.json.JsonValue.NULL;
    }
    
    @Override
    public javax.json.JsonValue toXJson() {
        return javax.json.JsonValue.NULL;
    }

    @Override
    public String toString() {
        return toJSON();
    }
    
    @Override
    public String toJSON() {
        return "null";
    }

    @Override
    public String toJSON(JSONFormat format) {
        return toJSON();
    }
    
    /**
     * JsonValueは一方的な比較
     * @param obj
     * @return 
     */
    @Override
    public boolean equals(Object obj) {
        return (obj != null && (obj instanceof JSONBaseNULL || obj == javax.json.JsonValue.NULL || obj == javax.json.JsonValue.NULL ));
            
    }
    
}
