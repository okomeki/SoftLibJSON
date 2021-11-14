package net.siisise.json2.jsonp;

import java.io.IOException;
import java.io.Reader;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import net.siisise.json.JSONValue;

/**
 * リーダー
 */
public class JSONPReader implements JsonReader {

    private final Reader rd;
    private final JSONValue json;

    JSONPReader(Reader reader, JSONValue json) {
        rd = reader;
        this.json = json;
    }

    @Override
    public JsonStructure read() {
        return (JsonStructure) json.toJson();
    }

    @Override
    public JsonValue readValue() {
        return json.toJson();
    }

    @Override
    public JsonObject readObject() {
        return (JsonObject) json.toJson();
    }

    @Override
    public JsonArray readArray() {
        return (JsonArray) json.toJson();
    }

    @Override
    public void close() {
        try {
            rd.close();
        } catch (IOException ex) {
            Logger.getLogger(JSONPReader.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

}
