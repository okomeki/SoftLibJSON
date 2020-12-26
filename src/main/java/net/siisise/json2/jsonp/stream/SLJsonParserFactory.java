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
import net.siisise.json2.JSON2;
import net.siisise.json2.JSON2Value;

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
        JSON2Value json = JSON2.valueOf(jobj);
        return new SLJsonParser(json);
    }

    @Override
    public JsonParser createParser(JsonArray jsonarray) {
        JSON2Value json = JSON2.valueOf(jsonarray);
        return new SLJsonParser(json);
    }

    @Override
    public Map<String, ?> getConfigInUse() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
}
