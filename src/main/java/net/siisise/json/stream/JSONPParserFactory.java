package net.siisise.json.stream;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Map;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.stream.JsonParser;
import javax.json.stream.JsonParserFactory;

/**
 *
 */
public class JSONPParserFactory implements JsonParserFactory {

    @Override
    public JsonParser createParser(Reader reader) {
        return new JSONPParser(reader);
    }

    @Override
    public JsonParser createParser(InputStream in) {
        return new JSONPParser(in);
    }

    @Override
    public JsonParser createParser(InputStream in, Charset chrst) {
        return new JSONPParser(new InputStreamReader(in,chrst));
    }

    @Override
    public JsonParser createParser(JsonObject json) {
        return new JSONPParser(json);
    }

    @Override
    public JsonParser createParser(JsonArray jsonarray) {
        return new JSONPParser(jsonarray);
    }

    @Override
    public Map<String, ?> getConfigInUse() {
        return new HashMap<>();
    }
    
}
