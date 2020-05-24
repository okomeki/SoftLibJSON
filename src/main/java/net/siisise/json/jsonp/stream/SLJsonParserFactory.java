package net.siisise.json.jsonp.stream;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.Charset;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.stream.JsonParser;
import javax.json.stream.JsonParserFactory;
import net.siisise.io.FileIO;
import net.siisise.json.JSON;
import net.siisise.json.JSONValue;

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
    public JsonParser createParser(JsonObject jobj) {
        JSONValue json = JSONValue.valueOf(jobj);
        return new SLJsonParser(json);
    }

    @Override
    public JsonParser createParser(JsonArray jsonarray) {
        JSONValue json = JSONValue.valueOf(jsonarray);
        return new SLJsonParser(json);
    }

    @Override
    public Map<String, ?> getConfigInUse() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
}
