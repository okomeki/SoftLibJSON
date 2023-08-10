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

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.lang.reflect.Type;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.LinkedList;
import java.util.Optional;
import javax.json.bind.Jsonb;
import javax.json.bind.JsonbConfig;
import javax.json.bind.JsonbException;
import net.siisise.bind.Rebind;
import net.siisise.bind.format.TypeFormat;
import net.siisise.io.FrontPacket;
import net.siisise.io.StreamFrontPacket;
import net.siisise.json.JSON;

/**
 * OMAPの該当機能を割り当てるだけ.
 * closeは必要なら有効に戻す.
 */
public class JSONB implements Jsonb {
    
    private JsonbConfig conf;
    
    LinkedList<Reader> readers = new LinkedList<>();
    LinkedList<Writer> writers = new LinkedList<>();
    LinkedList<InputStream> ins = new LinkedList<>();
    LinkedList<OutputStream> outs = new LinkedList<>();
    
    public JSONB() {
        
    }
    
    public JSONB(JsonbConfig config) {
        conf = config;
        
    }
    
    /**
     * JSON 文字列からオブジェクトの生成.
     * JSON Objectに変換してからBindでJava Objectにする
     * @param <T>
     * @param str
     * @param type
     * @return
     * @throws JsonbException 
     */
    @Override
    public <T> T fromJson(String str, Class<T> type) throws JsonbException {
        Object json = JSON.parse(str);
        return Rebind.valueOf(json, type);
    }

    /**
     * JSON文字列から Java Objectに変換する.
     * @param <T> 適度な型のJava Object
     * @param str JSON データ
     * @param runtimeType Java 型
     * @return
     * @throws JsonbException
     */
    @Override
    public <T> T fromJson(String str, Type runtimeType) throws JsonbException {
        Object json = JSON.parse(str);
        return Rebind.valueOf(json, runtimeType);
    }

    /**
     * JSON Reader列から Java Objectに変換する.
     * 
     * @param <T> 適度な型のJava Object
     * @param reader
     * @param type
     * @return
     * @throws JsonbException 
     */
    @Override
    public <T> T fromJson(Reader reader, Class<T> type) throws JsonbException {
        FrontPacket fp = new StreamFrontPacket(reader);
        Object json = JSON.parse(fp);
        readers.add(reader);
        return Rebind.valueOf(json, type);
        
    }

    @Override
    public <T> T fromJson(Reader reader, Type runtimeType) throws JsonbException {
        FrontPacket fp = new StreamFrontPacket(reader);
        Object json = JSON.parse(fp);
        readers.add(reader);
        return Rebind.valueOf(json, runtimeType);
    }

    @Override
    public <T> T fromJson(InputStream stream, Class<T> type) throws JsonbException {
        FrontPacket fp = new StreamFrontPacket(stream);
        Object json = JSON.parse(fp);
        ins.add(stream);
        return Rebind.valueOf(json, type);
    }

    @Override
    public <T> T fromJson(InputStream stream, Type runtimeType) throws JsonbException {
        FrontPacket fp = new StreamFrontPacket(stream);
        Object json = JSON.parse(fp);
        ins.add(stream);
        return Rebind.valueOf(json, runtimeType);
    }

    @Override
    public String toJson(Object object) throws JsonbException {
        TypeFormat<String> format = JSON.NOBR;
        if ( conf != null ) {
            Optional<Object> formatted = conf.getProperty("jsonb.to.json.formatted");
            if ( formatted.isPresent() && formatted.get().equals(Boolean.TRUE)) {
                format = JSON.TAB_MINESC;
            }
        }
        return Rebind.valueOf(object, format);
    }

    /**
     * JSONValue系(JsonValue互換)に変換する
     * @param object ソース
     * @param runtimeType ソースの型 未使用
     * @return JSONValue なJSON
     * @throws JsonbException 未定
     */
    @Override
    public String toJson(Object object, Type runtimeType) throws JsonbException {
        return toJson(object);
    }

    /**
     * Java Object をJSONにしてWriterに書き出す。
     * 
     * @param object java object
     * @param writer 出力先
     * @throws JsonbException 出力先のエラー
     */
    @Override
    public void toJson(Object object, Writer writer) throws JsonbException {
        try {
            writer.write(toJson(object));
            writer.flush();
            writers.add(writer);
        } catch (IOException ex) {
            throw new JsonbException(ex.getLocalizedMessage(),ex);
        }
    }

    /**
     * Java Object をJSONにしてWriterに書き出す。
     * @param object object
     * @param runtimeType ソースの型 未使用
     * @param writer 出力先
     * @throws JsonbException 
     */
    @Override
    public void toJson(Object object, Type runtimeType, Writer writer) throws JsonbException {
        try {
            writer.write(toJson(object));
            writer.flush();
            writers.add(writer);
        } catch (IOException ex) {
            throw new JsonbException(ex.getLocalizedMessage(),ex);
        }
    }

    @Override
    public void toJson(Object object, OutputStream stream) throws JsonbException {
        try {
            Charset charset = StandardCharsets.UTF_8;
            Optional<Object> encopt = conf.getProperty("jsonb.to.json.encoding");
            if ( encopt.isPresent() ) {
                charset = Charset.forName((String)encopt.get());
            }
            stream.write(toJson(object).getBytes(charset));
            stream.flush();
            outs.add(stream);
        } catch (IOException e) {
            throw new JsonbException(e.getLocalizedMessage(), e);
        }
    }

    @Override
    public void toJson(Object object, Type runtimeType, OutputStream stream) throws JsonbException {
        toJson(object, stream);
    }

    /**
     * 使ったものを閉じるかもしれない
     */
    @Override
    public void close() {
        while ( !readers.isEmpty() ) {
            try {
                readers.remove().close();
            } catch (IOException e) {
            }
        }
        while ( !writers.isEmpty() ) {
            try {
                writers.remove().close();
            } catch ( IOException e ) {
            }
        }
        while ( !ins.isEmpty() ) {
            try {
                ins.remove().close();
            } catch ( IOException e ) {
            }
        }
        while ( !outs.isEmpty() ) {
            try {
                outs.remove().close();
            } catch ( IOException e ) {
            }
        }
    }
    
}
