package net.siisise.json.jsonxp;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonReader;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import net.siisise.io.FrontPacket;
import net.siisise.json.JSON;

/**
 * リーダー
 */
public class JSONXReader implements JsonReader {

    private final FrontPacket fp;

    JSONXReader(FrontPacket front) {
        fp = front;
    }

    @Override
    public JsonStructure read() {
        return (JsonStructure) JSON.parseWrap(fp).toJson();
    }

    @Override
    public JsonValue readValue() {
        return JSON.parseWrap(fp).toJson();
    }

    @Override
    public JsonObject readObject() {
        return (JsonObject) JSON.parseWrap(fp).toJson();
    }

    @Override
    public JsonArray readArray() {
        return (JsonArray) JSON.parseWrap(fp).toJson();
    }

    @Override
    public void close() {
        try {
            fp.getInputStream().close();
        } catch (IOException ex) {
            Logger.getLogger(JSONXReader.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

}
