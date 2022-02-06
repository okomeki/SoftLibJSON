package net.siisise.json.jsonp;

import java.io.IOException;
import java.io.Writer;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import javax.json.JsonWriter;
import net.siisise.json.JSON;

/**
 *
 */
public class JSONPWriter implements JsonWriter {

    private final Writer writer;

    public JSONPWriter(Writer writer) {
        this.writer = writer;
    }

    @Override
    public void writeArray(JsonArray array) {
        try {
            writer.write(JSON.valueOf(array).toJSON());
            writer.flush();
        } catch (IOException ex) {
            Logger.getLogger(JSONPWriter.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }
    }

    @Override
    public void writeObject(JsonObject object) {
        try {
            writer.write(JSON.valueOf(object).toJSON());
            writer.flush();
        } catch (IOException ex) {
            Logger.getLogger(JSONPWriter.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }
    }

    @Override
    public void write(JsonStructure value) {
        try {
            writer.write(JSON.valueOf(value).toJSON());
            writer.flush();
        } catch (IOException ex) {
            Logger.getLogger(JSONPWriter.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }
    }

    @Override
    public void write(JsonValue value) {
        try {
            writer.write(JSON.valueOf(value).toJSON());
            writer.flush();
        } catch (IOException ex) {
            Logger.getLogger(JSONPWriter.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
        }
    }

    @Override
    public void close() {
        try {
            writer.close();
        } catch (IOException ex) {
            Logger.getLogger(JSONPWriter.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
}
