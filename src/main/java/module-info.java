/*
 * Copyright 2024 okome.
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

module net.siisise.json {
    requires java.json;
    requires java.json.bind;
    requires java.logging;
//    uses javax.json.spi.JsonProvider;
//    uses net.siisise.bind.format.TypeFormat;
    requires net.siisise;
    requires net.siisise.abnf;
    requires net.siisise.rebind;
    exports net.siisise.json;
    exports net.siisise.json.base;
    exports net.siisise.json.bind.target;
    exports net.siisise.json.parser;
    exports net.siisise.json.stream;
    exports net.siisise.msgpack;
    provides javax.json.bind.spi.JsonbProvider
            with net.siisise.json.bind.spi.JSONBProvider;
    provides javax.json.spi.JsonProvider
            with net.siisise.json.spi.JSONPProvider;
    provides net.siisise.bind.format.TypeFormat
            with net.siisise.json.bind.target.JSONConvert,
            net.siisise.json.bind.target.JSONFormat,
            net.siisise.json.bind.target.JsonpConvert,
            net.siisise.json.bind.target.MessagePackConvert;
    provides net.siisise.bind.UnbindList with net.siisise.json.unbind.JSONUnbindList;
}
