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
 * JSON boolean.
 * JSON Pに定義があるので近いところにあわせておく。
 */
public class JSON2Boolean implements JSON2Value,JsonValue {
    
    public static final JSON2Boolean TRUE = new JSON2Boolean(true);
    public static final JSON2Boolean FALSE = new JSON2Boolean(false);

    private final boolean bool;
    
    public JSON2Boolean(boolean b) {
        bool = b;
    }
    
    public static JSON2Boolean valieOf(boolean b) {
        return b ? TRUE : FALSE;
    }

    @Override
    public <T> T typeMap(Type type) {
        return (T)OMAP.typeBoolean(bool, type);
    }

    @Override
    public JsonValue toJson() {
        return bool ? JsonValue.TRUE : JsonValue.FALSE;
    }
    
    @Override
    public String toString() {
        return toJSON();
    }
    
    @Override
    public String toJSON() {
        return Boolean.toString(bool);
    }
    
    @Override
    public String toJSON(JSON2Format format) {
        return Boolean.toString(bool);
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
        if ( obj instanceof JSON2Boolean ) {
            return bool == (Boolean)((JSON2Boolean) obj).map();
        } else if ( obj == JsonValue.TRUE ) {
            return bool;
        } else if ( obj == JsonValue.FALSE ) {
            return !bool;
//        } else if ( obj instanceof Boolean ) {
//            return bool == ((Boolean)obj).booleanValue();
        }
        return false;
    }
    
}
