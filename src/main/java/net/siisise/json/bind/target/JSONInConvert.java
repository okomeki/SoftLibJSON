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

import java.util.Collection;
import java.util.Map;
import net.siisise.bind.format.TypeFormat;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONObject;

/**
 *
 */
public class JSONInConvert implements TypeFormat<Object> {

    @Override
    public Object nullFormat() {
        return null;
    }

    @Override
    public Object booleanFormat(boolean bool) {
        return bool;
    }

    @Override
    public Number numberFormat(Number num) {
        return num;
    }

    @Override
    public String stringFormat(String str) {
        return str;
    }

    @Override
    public Map mapFormat(Map map) {
        return new JSONObject(map);
    }

    @Override
    public Object collectionFormat(Collection col) {
        return new JSONArray(col);
    }
    
}
