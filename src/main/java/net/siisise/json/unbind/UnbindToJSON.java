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
package net.siisise.json.unbind;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Type;
import net.siisise.bind.Rebind;
import net.siisise.bind.TypeUnbind;
import net.siisise.bind.format.TypeFormat;
import net.siisise.json.JSON;
import net.siisise.json.JSONValue;

/**
 * Jakarta EEも対応しているはず.
 */
public class UnbindToJSON implements TypeUnbind {

    @Override
    public Type[] getSrcTypes() {
        return new Type[] { Object.class };
    }

    @Override
    public <T> T valueOf(Object src, TypeFormat<T> format) {
        Class<? extends Object> cls = src.getClass();
        if ( JSONValue.class.isAssignableFrom(cls)) { // toJSON を持っているので除外
            return (T)this;
        }
        try {
            Method toJSON = cls.getMethod("toJSON");
            //Type retType = toJSON.getGenericReturnType();
            Class<?> retClass = toJSON.getReturnType();
            if ( String.class.isAssignableFrom(retClass)) {
                return Rebind.valueOf(JSON.parse((String)toJSON.invoke(src)), format);
            }
            return Rebind.valueOf(toJSON.invoke(src), format);
        } catch (NoSuchMethodException ex) {
        } catch (SecurityException | IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
            throw new IllegalStateException(ex);
        }
        return (T)this;
    }
    
}
