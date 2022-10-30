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
package net.siisise.json.bind.source;

import net.siisise.json.JSONString;
import net.siisise.json.bind.MtoConvert;
import net.siisise.json.bind.OMConvert;

/**
 *
 */
public class JSONStringM implements OMConvert {

    @Override
    public Class[] getSrcClasses() {
        return new Class[] { javax.json.JsonString.class, javax.json.JsonString.class, JSONString.class, String.class, CharSequence.class };
    }

    @Override
    public Object valueOf(Object obj, MtoConvert outConvert) {
        if ( obj instanceof javax.json.JsonString ) { // JSON2String も該当
            obj = ((javax.json.JsonString)obj).getString();
//        } else if ( obj instanceof UUID ) {
//            obj = ((UUID)obj).toString();
        } else if ( obj instanceof javax.json.JsonString ) {
            obj = ((javax.json.JsonString)obj).getString();
//        } else if ( obj instanceof UUID ) {
//            obj = ((UUID)obj).toString();
        }
        if ( obj instanceof String ) {
            return outConvert.stringValue((String)obj);
        }
        if ( obj instanceof CharSequence ) {
            return outConvert.stringValue((CharSequence)obj);
        }
        return this;
    }
    
}
