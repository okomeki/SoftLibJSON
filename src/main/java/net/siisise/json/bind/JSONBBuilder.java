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
package net.siisise.json.bind;

import javax.json.bind.JsonbBuilder;
import javax.json.bind.JsonbConfig;
import javax.json.spi.JsonProvider;

/**
 * JsonbBuilder のなんとなくの実装.
 */
public class JSONBBuilder implements JsonbBuilder {
    
    private JsonbConfig conf;

    @Override
    public JsonbBuilder withConfig(JsonbConfig config) {
        conf = config;
        return this;
    }

    /**
     * JSON P はSoftLibJSON 専用なので未対応
     * @param jsonpProvider 無効
     * @return JsonbのBuilder
     */
    @Override
    public JsonbBuilder withProvider(JsonProvider jsonpProvider) {
        return this;
    }

    @Override
    public JSONB build() {
        return new JSONB();
    }
    
}
