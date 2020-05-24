package net.siisise.json.jsonp;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.StringWriter;
import java.nio.charset.Charset;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonReader;
import javax.json.JsonReaderFactory;
import net.siisise.io.FileIO;
import net.siisise.json.JSON;
import net.siisise.json.JSONValue;

/**
 *
 */
public class JSONPReaderFactory implements JsonReaderFactory {



    @Override
    public JsonReader createReader(Reader reader) {
        try {
            StringWriter out = new StringWriter();
            FileIO.io(reader, out);
            JSONValue json = JSON.parse(out.toString());
            return new JSONPReader(reader,json);
        } catch (IOException ex) {
            Logger.getLogger(JSONPReaderFactory.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException("Not supported yet.");
        }
    }

    @Override
    public JsonReader createReader(InputStream in) {
        return createReader(in, Charset.forName("utf-8"));
    }

    @Override
    public JsonReader createReader(InputStream in, Charset charset) {
        return createReader(new InputStreamReader(in, charset));
    }

    @Override
    public Map<String, ?> getConfigInUse() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }
    
}
