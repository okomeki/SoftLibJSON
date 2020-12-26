package net.siisise.json2.jsonp;

import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;
import javax.json.stream.JsonGenerator;
import javax.json.stream.JsonGeneratorFactory;

/**
 *
 */
public class JSONPGeneratorFactory implements JsonGeneratorFactory {

    @Override
    public JsonGenerator createGenerator(Writer writer) {
        return new JSONPGenerator(writer);
    }

    @Override
    public JsonGenerator createGenerator(OutputStream out) {
        return createGenerator(out, Charset.forName("utf-8"));
    }

    @Override
    public JsonGenerator createGenerator(OutputStream out, Charset charset) {
        return new JSONPGenerator(new OutputStreamWriter(out, charset));
    }

    @Override
    public Map<String, ?> getConfigInUse() {
        return new HashMap<>();
    }

}
