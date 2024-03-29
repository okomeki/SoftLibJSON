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

import javax.json.JsonValue;
import net.siisise.bind.format.TypeFormat;

/**
 * JSON boolean.
 * JSON Pに定義があるので近いところにあわせておく。
 */
public class JSONBoolean implements JSONValue,JsonValue {
    
    public static final JSONBoolean TRUE = new JSONBoolean(true);
    public static final JSONBoolean FALSE = new JSONBoolean(false);

    private final boolean bool;
    
    public JSONBoolean(boolean b) {
        bool = b;
    }
    
    public static JSONBoolean valieOf(boolean b) {
        return b ? TRUE : FALSE;
    }

    @Override
    public String toString() {
        return toJSON();
    }
    
    @Override
    public <V> V rebind(TypeFormat<V> format) {
        return format.booleanFormat(bool);
    }

    @Override
    public <T> T map() {
        return (T)Boolean.valueOf(bool);
    }

    @Override
    public ValueType getValueType() {
        return bool ? JsonValue.ValueType.TRUE : JsonValue.ValueType.FALSE;
    }
    
    @Override
    public boolean equals(Object obj) {
        if ( obj instanceof JSONBoolean ) {
            return bool == (Boolean)((JSONBoolean) obj).map();
        } else if ( obj == JsonValue.TRUE ) {
            return bool;
        } else if ( obj == JsonValue.FALSE ) {
            return !bool;
//        } else if ( obj instanceof Boolean ) {
//            return bool == ((Boolean)obj).booleanFormat();
        }
        return false;
    }
    
}
