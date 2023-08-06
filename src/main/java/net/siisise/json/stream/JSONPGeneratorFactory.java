package net.siisise.json.stream;

import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import javax.json.stream.JsonGenerator;
import javax.json.stream.JsonGeneratorFactory;
import net.siisise.json.bind.target.JSONFormat;
import net.siisise.json.JSON;

/**
 *
 */
public class JSONPGeneratorFactory implements JsonGeneratorFactory {
    
    final JSONFormat format;

    public JSONPGeneratorFactory(Map<String, ?> config) {
        format = (config != null && config.containsKey(JsonGenerator.PRETTY_PRINTING)) ? JSON.TAB : JSON.NOBR;
    }

    @Override
    public JsonGenerator createGenerator(Writer writer) {
        return new JSONPGenerator(writer, format);
    }

    @Override
    public JsonGenerator createGenerator(OutputStream out) {
        return createGenerator(out, StandardCharsets.UTF_8);
    }

    @Override
    public JsonGenerator createGenerator(OutputStream out, Charset charset) {
        return new JSONPGenerator(new OutputStreamWriter(out, charset), format);
    }

    @Override
    public Map<String, ?> getConfigInUse() {
        Map config = new HashMap<>();
        if ( format == JSON.TAB ) {
            config.put(JsonGenerator.PRETTY_PRINTING, true);
        }
        return config;
    }

}
