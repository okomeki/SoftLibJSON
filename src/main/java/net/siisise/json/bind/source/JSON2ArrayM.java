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

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
//import net.siisise.io.BASE64;
import net.siisise.json.JSON2Array;
import net.siisise.json.bind.MtoConvert;
import net.siisise.json.bind.OMConvert;

/**
 * Array and List
 * 配列とリスト
 */
public class JSON2ArrayM implements OMConvert {

    @Override
    public Class[] getSrcClasses() {
        return new Class[] { Collection.class, ArrayList.class };
    }

    /**
     * 配列も一度Collectionにまとめてから出力側に振る
     * @param src Collection(List), 配列
     * @param outConvert 出力先変換器
     * @return 
     */
    @Override
    public Object valueOf(Object src, MtoConvert outConvert) {
        Class cls = src.getClass();
        if ( cls.isArray() ) { // 配列をCollectionに変更する
            Class ar = cls.getComponentType();
            if (ar.isPrimitive()) {
                List cnv;
                int len = Array.getLength(src);
                // byte[], char[] は別にするかも
                /*
                if ( ar == Byte.TYPE ) {
                    BASE64 b64 = new BASE64(BASE64.URL,0);
                    String b64src = b64.encode((byte[])src);
                    
                } else if ( ar == Character.TYPE ) {
                    
                }
                */
                cnv = new JSON2Array();
                for ( int i = 0; i < len; i++ ) {
                    cnv.add(Array.get(src, i));
                }
                src = cnv;
            } else {
                src = Arrays.asList((Object[]) src);
            }
        }
        if (src instanceof Collection) {
            return outConvert.listValue((Collection) src);
        }
        return this;
    }
}
