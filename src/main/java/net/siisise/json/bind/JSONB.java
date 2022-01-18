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
import java.nio.charset.StandardCharsets;
import java.util.LinkedList;
import javax.json.bind.Jsonb;
import javax.json.bind.JsonbConfig;
import javax.json.bind.JsonbException;
import net.siisise.io.FrontPacket;
import net.siisise.io.StreamFrontPacket;
import net.siisise.json.JSON2;
import net.siisise.json.JSON2Value;

/**
 * OMAPの該当機能を割り当てるだけ
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
    
    @Override
    public <T> T fromJson(String str, Class<T> type) throws JsonbException {
        Object json = JSON2.parse(str);
        return OMAP.valueOf(json, type);
    }

    /**
     * 
     */
    @Override
    public <T> T fromJson(String str, Type runtimeType) throws JsonbException {
        Object json = JSON2.parse(str);
        return OMAP.valueOf(json, runtimeType);
    }

    @Override
    public <T> T fromJson(Reader reader, Class<T> type) throws JsonbException {
        FrontPacket fp = new StreamFrontPacket(reader);
        Object json = JSON2.parse(fp);
        readers.add(reader);
        return OMAP.valueOf(json, type);
        
    }

    @Override
    public <T> T fromJson(Reader reader, Type runtimeType) throws JsonbException {
        FrontPacket fp = new StreamFrontPacket(reader);
        Object json = JSON2.parse(fp);
        readers.add(reader);
        return OMAP.valueOf(json, runtimeType);
    }

    @Override
    public <T> T fromJson(InputStream stream, Class<T> type) throws JsonbException {
        FrontPacket fp = new StreamFrontPacket(stream);
        Object json = JSON2.parse(fp);
        ins.add(stream);
        return OMAP.valueOf(json, type);
    }

    @Override
    public <T> T fromJson(InputStream stream, Type runtimeType) throws JsonbException {
        FrontPacket fp = new StreamFrontPacket(stream);
        Object json = JSON2.parse(fp);
        ins.add(stream);
        return OMAP.valueOf(json, runtimeType);
    }

    @Override
    public String toJson(Object object) throws JsonbException {
        return OMAP.valueOf(object, JSON2Value.class).toString();
    }

    @Override
    public String toJson(Object object, Type runtimeType) throws JsonbException {
        return OMAP.valueOf(object, JSON2Value.class).toString();
    }

    @Override
    public void toJson(Object object, Writer writer) throws JsonbException {
        try {
            writer.write(JSON2.valueOf(object).toString());
            writer.flush();
            writers.add(writer);
        } catch (IOException ex) {
            throw new JsonbException(ex.getLocalizedMessage(),ex);
        }
    }

    @Override
    public void toJson(Object object, Type runtimeType, Writer writer) throws JsonbException {
        try {
            writer.write(JSON2.valueOf(object).toString());
            writer.flush();
            writers.add(writer);
        } catch (IOException ex) {
            throw new JsonbException(ex.getLocalizedMessage(),ex);
        }
    }

    @Override
    public void toJson(Object object, OutputStream stream) throws JsonbException {
        try {
            stream.write(JSON2.valueOf(object).toString().getBytes(StandardCharsets.UTF_8));
            stream.flush();
            outs.add(stream);
        } catch (IOException ex) {
            throw new JsonbException(ex.getLocalizedMessage(),ex);
        }
    }

    @Override
    public void toJson(Object object, Type runtimeType, OutputStream stream) throws JsonbException {
        try {
            stream.write(JSON2.valueOf(object).toString().getBytes(StandardCharsets.UTF_8));
            stream.flush();
            outs.add(stream);
        } catch (IOException ex) {
            throw new JsonbException(ex.getLocalizedMessage(),ex);
        }
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
