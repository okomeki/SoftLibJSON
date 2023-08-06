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

import javax.json.JsonNumber;
import javax.json.JsonValue.ValueType;
import net.siisise.json.base.JSONBaseNumber;

/**
 * JSON Number.
 * Number を扱う.
 * 値の変更は不可.
 * データ型は特定のものを想定していないのでいろいろ。
 * これもNumver型になれるので多重ラップしないよう注意。
 */
public class JSONNumber extends JSONBaseNumber implements JSONValue,JsonNumber {

    private static final long serialVersionUID = 1L;
    
    public JSONNumber(Number num) {
        super(num);
    }

    /**
     *
     * @return
     */
    @Override
    public JSONNumber toJson() {
        return this;
    }
    
    @Override
    public ValueType getValueType() {
        return ValueType.NUMBER;
    }
    
    @Override
    public boolean equals(Object o) {
        if ( o instanceof JSONBaseNumber ) {
            return map().equals(((JSONValue)o).map());
        } else if ( o instanceof Number ) {
            return number.equals(o);
        }
        return false;
    }
    
}
