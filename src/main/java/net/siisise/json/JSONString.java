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

import javax.json.JsonString;
import javax.json.JsonValue;
import net.siisise.json.base.JSONBaseString;

/**
 * JSON String.
 */
public class JSONString extends JSONBaseString implements JsonString {
    
     public JSONString(CharSequence val) {
        super(val);
    }

    public JSONString(String val) {
        super(val);
    }

    @Override
    public boolean equals(Object obj) {
        if ( obj instanceof JsonString) {
            return getString().equals(((JsonString) obj).getString());
        }
        if ( obj instanceof javax.json.JsonString) {
            return getString().equals(((javax.json.JsonString) obj).getString());
        }
        return false;
    }

    @Override
    public JsonValue toJson() {
        return this;
    }

    @Override
    public ValueType getValueType() {
        return ValueType.STRING;
    }
}
