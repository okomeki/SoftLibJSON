package net.siisise.json2.jsonp;

import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Map;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;

/**
 *
 */
public class JSONPWriterFactory implements JsonWriterFactory {

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
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
}
