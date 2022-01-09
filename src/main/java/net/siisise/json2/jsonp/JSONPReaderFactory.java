package net.siisise.json2.jsonp;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;
import javax.json.JsonReader;
import javax.json.JsonReaderFactory;
import net.siisise.io.FrontPacket;
import net.siisise.io.StreamFrontPacket;

/**
 *
 */
public class JSONPReaderFactory implements JsonReaderFactory {

    private final Map<String, ?> config;

    public JSONPReaderFactory(Map<String, ?> map) {
        config = map;
    }

    @Override
    public JsonReader createReader(Reader reader) {
        FrontPacket fp = new StreamFrontPacket(reader);
        return new JSONPReader(fp);
    }
    
    @Override
    public JsonReader createReader(InputStream in) {
        FrontPacket fp = new StreamFrontPacket(in);
        return new JSONPReader(fp);
    }

    @Override
    public JsonReader createReader(InputStream in, Charset charset) {
        return createReader(new InputStreamReader(in, charset));
    }

    @Override
    public Map<String, ?> getConfigInUse() {
        return new HashMap<>(config);
    }
    
}
