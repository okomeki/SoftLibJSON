package net.siisise.json.jsonp;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import net.siisise.io.FrontPacket;
import net.siisise.json.JSON2;

/**
 * リーダー
 */
public class JSONPReader implements JsonReader {

    private final FrontPacket fp;

    JSONPReader(FrontPacket front) {
        fp = front;
    }

    @Override
    public JsonStructure read() {
        return (JsonStructure) JSON2.parseWrap(fp).toJson();
    }

    @Override
    public JsonValue readValue() {
        return JSON2.parseWrap(fp).toJson();
    }

    @Override
    public JsonObject readObject() {
        return (JsonObject) JSON2.parseWrap(fp).toJson();
    }

    @Override
    public JsonArray readArray() {
        return (JsonArray) JSON2.parseWrap(fp).toJson();
    }

    @Override
    public void close() {
        try {
            fp.getInputStream().close();
        } catch (IOException ex) {
            Logger.getLogger(JSONPReader.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

}
