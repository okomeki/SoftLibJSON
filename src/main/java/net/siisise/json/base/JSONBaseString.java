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
import java.util.Objects;
import java.util.stream.IntStream;
import net.siisise.bind.format.TypeFormat;
import net.siisise.json.JSONString;
import net.siisise.json.JSONValue;
import net.siisise.json.bind.OMAP;

/**
 * 両方対応する元
 */
public class JSONBaseString implements JSONValue,CharSequence {

    private final String value;

    protected JSONBaseString(CharSequence val) {
        value = val.toString();
    }

    protected JSONBaseString(String val) {
        value = val;
    }

    @Override
    public <T> T map() {
        return (T)value;
    }

    @Override
    public <T> T typeMap(Type type) {
//        if ( type == javax.json.JsonString.class || type == javax.json.JsonValue.class ) {
//            return (T) toJson();
//        }
        return (T)OMAP.typeString(value, type);
    }

    @Override
    public String toString() {
        return value;
    }

    @Override
    public <V> V toJSON(TypeFormat<V> format) {
        return format.stringFormat(value);
    }
    
    @Override
    public javax.json.JsonValue toJson() {
        return new JSONString(value);
    }

    public String getString() {
        return value;
    }

    public CharSequence getChars() {
        return value;
    }

    @Override
    public int length() {
        return value.length();
    }

    @Override
    public char charAt(int index) {
        return value.charAt(index);
    }

    @Override
    public CharSequence subSequence(int start, int end) {
        return value.subSequence(start, end);
    }
    
    @Override
    public IntStream chars() {
        return value.chars();
    }
    
    @Override
    public IntStream codePoints() {
        return value.codePoints();
    }

    @Override
    public boolean equals(Object obj) {
        if ( obj instanceof javax.json.JsonString) {
            return value.equals(((javax.json.JsonString) obj).getString());
        }
        if ( obj instanceof javax.json.JsonString) {
            return value.equals(((javax.json.JsonString) obj).getString());
        }
        return false;
    }

    @Override
    public int hashCode() {
        return 83 * 7 + Objects.hashCode(this.value);
    }
}
