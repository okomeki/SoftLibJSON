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

import net.siisise.bind.format.TypeFormat;
import net.siisise.json.JSONValue;

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
    public String toString() {
        return toJSON();
    }
    
    @Override
    public <V> V rebind(TypeFormat<V> format) {
        return format.nullFormat();
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
