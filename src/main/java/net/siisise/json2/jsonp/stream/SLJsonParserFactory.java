package net.siisise.json2.jsonp.stream;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.Charset;
import java.util.Map;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.stream.JsonParser;
import javax.json.stream.JsonParserFactory;

/**
 *
 */
public class SLJsonParserFactory implements JsonParserFactory {

    @Override
    public JsonParser createParser(Reader reader) {
        return new SLJsonParser(reader);
    }

    @Override
    public JsonParser createParser(InputStream in) {
        return new SLJsonParser(in);
    }

    @Override
    public JsonParser createParser(InputStream in, Charset chrst) {
        return new SLJsonParser(new InputStreamReader(in,chrst));
    }

    @Override
    public JsonParser createParser(JsonObject json) {
        return new SLJsonParser(json);
    }

    @Override
    public JsonParser createParser(JsonArray jsonarray) {
        return new SLJsonParser(jsonarray);
    }

    @Override
    public Map<String, ?> getConfigInUse() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
}
