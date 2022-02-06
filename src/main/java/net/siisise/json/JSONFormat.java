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

/**
 * 改行をなんとかするだけ。
 * JSON2Value#toString(JSON2Format) で使う
 */
public class JSONFormat {
    
    public final String crlf;
    public final String tab;

    /**
     * 改行やタブを入れるか省略するか決める要素
     * @param crlf 改行コードに相当する部分
     * @param tab タブに相当する部分
     */
    JSONFormat(String crlf, String tab) {
        this.crlf = crlf;
        this.tab = tab;
    }
}
