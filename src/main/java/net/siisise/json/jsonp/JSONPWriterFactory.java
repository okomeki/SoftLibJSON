package net.siisise.json.jsonp;

import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;

/**
 *
 */
public class JSONPWriterFactory implements JsonWriterFactory {

    private final Map<String, ?> config;

    public JSONPWriterFactory(Map<String, ?> map) {
        config = map;
    }

    @Override
    public JsonWriter createWriter(Writer writer) {
        return new JSONPWriter(writer);
    }

    @Override
    public JsonWriter createWriter(OutputStream out) {
        return createWriter(out,StandardCharsets.UTF_8);
    }

    @Override
    public JsonWriter createWriter(OutputStream out, Charset charset) {
        return createWriter(new OutputStreamWriter(out,charset));
    }

    @Override
    public Map<String, ?> getConfigInUse() {
        return new HashMap<>(config);
    }
    
}
