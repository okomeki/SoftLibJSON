package net.siisise.json.jsonp.stream;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;
import javax.json.stream.JsonLocation;
import javax.json.stream.JsonParser;
import net.siisise.io.FileIO;
import net.siisise.json2.JSON2;
import net.siisise.json2.JSON2Array;
import net.siisise.json2.JSON2Boolean;
import net.siisise.json2.JSON2NULL;
import net.siisise.json2.JSON2Number;
import net.siisise.json2.JSON2Object;
import net.siisise.json2.JSON2String;
import net.siisise.json2.JSON2Value;

/**
 * ABNF Parserを使っているのでこちらは未対応.
 * @deprecated まだ使えない
 */
public class SLJsonParser implements JsonParser {

    JSON2Value json;
    Reader rd;
    
    
    
    List<Next> nexts = new ArrayList<>();
    
    class Next {
        JSON2Value json;
        Event state;
        int index;
        
        Next(JSON2Value json) {
            this.json = json;
            index = 0;
        }
    }

    public SLJsonParser(Reader reader) {
        try {
            StringWriter out = new StringWriter();
            FileIO.io(reader, out);
            json = JSON2.parseWrap(out.toString());
            rd = reader;
            nexts.add(0,new Next(json));
        } catch (IOException ex) {
            Logger.getLogger(SLJsonParser.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException();
        }
    }

    public SLJsonParser(InputStream reader) {
        try {
            byte[] data = FileIO.binRead(reader);
            json = JSON2.parseWrap(data);
            rd = new InputStreamReader(reader);
            nexts.add(0,new Next(json));
        } catch (IOException ex) {
            Logger.getLogger(SLJsonParser.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException();
        }
    }
    
    public SLJsonParser(JSON2Value json) {
        this.json = json;
    }

    @Override
    public boolean hasNext() {
        return !nexts.isEmpty();
    }
    
    Next current;

    @Override
    public Event next() {
        if ( current == null ) {
            current = nexts.get(0);
        } else if ( nexts.get(0).json instanceof JSON2Array || nexts.get(0).json instanceof JSON2Object ) {
        }
        
        
        JSON2Value json = current.json;
        if ( json instanceof JSON2Array ) {
            current.index++;
            if ( current.index == 1 ) {
                return Event.START_ARRAY;
            } else if ( current.index <= ((JSON2Array)json).size() +1 ) {
                current = new Next(((JSON2Array) json).getJSON(Integer.toString(current.index-2)));
                //nexts.add(0, current);
                return next();
            } else if ( current.index == ((JSON2Array)json).size() +2 ) {
                nexts.remove(0);
                return Event.END_ARRAY;
            }
        } else if ( json instanceof JSON2Object ) {
            nexts.add(new Next(JSON2.valueOf(((JSON2Array) json).get(current.index))));
            return Event.START_OBJECT;
        } else if ( json instanceof JSON2String ) {
            Next n = nexts.remove(0);
            return Event.VALUE_STRING;
        } else if ( json instanceof JSON2Boolean ) {
            Next n = nexts.remove(0);
            return ((Boolean)json.map()) ? Event.VALUE_TRUE :  Event.VALUE_FALSE;
        } else if ( json instanceof JSON2NULL ) {
            Next n = nexts.remove(0);
            return Event.VALUE_NULL;
        } else if ( json instanceof JSON2Number ) {
            Next n = nexts.remove(0);
            return Event.VALUE_NUMBER;
        }
        
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
    public JsonObject getObject() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
    @Override
    public JsonValue getValue() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public JsonArray getArray() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
    @Override
    public Stream<JsonValue> getArrayStream() {
        return getArray().stream();
    }

    @Override
    public Stream<Map.Entry<String,JsonValue>> getObjectStream() {
        return getObject().entrySet().stream();
    }


    @Override
    public Stream<JsonValue> getValueStream() {
        return getArray().stream();
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
