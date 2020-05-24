package net.siisise.json.jsonp.stream;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.stream.JsonLocation;
import javax.json.stream.JsonParser;
import net.siisise.io.FileIO;
import net.siisise.json.JSON;
import net.siisise.json.JSONValue;

/**
 *
 */
public class SLJsonParser implements JsonParser {

    JSONValue json;
    Reader rd;

    public SLJsonParser(Reader reader) {
        try {
            StringWriter out = new StringWriter();
            FileIO.io(reader, out);
            json = JSON.parse(out.toString());
            rd = reader;
        } catch (IOException ex) {
            Logger.getLogger(SLJsonParser.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException();
        }
    }

    public SLJsonParser(InputStream reader) {
        try {
            byte[] data = FileIO.binRead(reader);
            json = JSON.parse(data);
            rd = new InputStreamReader(reader);
        } catch (IOException ex) {
            Logger.getLogger(SLJsonParser.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException();
        }
    }
    
    public SLJsonParser(JSONValue json) {
        this.json = json;
    }

    @Override
    public boolean hasNext() {
        
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public Event next() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public String getString() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public boolean isIntegralNumber() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public int getInt() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public long getLong() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public BigDecimal getBigDecimal() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public JsonLocation getLocation() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void close() {
        if ( rd != null ) {
            try {
                rd.close();
            } catch (IOException ex) {
                Logger.getLogger(SLJsonParser.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }
    
}
