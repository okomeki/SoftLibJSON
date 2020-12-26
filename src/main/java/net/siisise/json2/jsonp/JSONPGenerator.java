package net.siisise.json2.jsonp;

import java.io.IOException;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.json.JsonValue;
import javax.json.stream.JsonGenerator;
import net.siisise.json.JSON;
import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONNULL;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONString;
import net.siisise.json.JSONValue;

/**
 * タブ未対応
 */
public class JSONPGenerator implements JsonGenerator {

    private final Writer out;

    JSONPGenerator up;
    boolean first = true;

    String CRLF = "\r\n";

    List<String> closeCode = new ArrayList();

    JSONPGenerator(Writer writer) {
        out = writer;
    }

    JSONPGenerator(JSONPGenerator g, Writer w) {
        up = g;
        out = w;
    }

    @Override
    public JsonGenerator writeStartObject() {
        try {
            writeSeparator();
            out.write("{");
            closeCode.add("}");
            out.write(CRLF);
            return new JSONPGenerator(this, out);
        } catch (IOException ex) {
            Logger.getLogger(JSONPGenerator.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException();
        }
    }

    @Override
    public JsonGenerator writeStartObject(String name) {
        try {
            writeSeparator();
            writeKey(name);
            out.write("{");
            out.write(CRLF);
            return new JSONPGenerator(this, out);
        } catch (IOException ex) {
            Logger.getLogger(JSONPGenerator.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException();
        }
    }

    void writeSeparator() throws IOException {
        if (first) {
            first = false;
        } else {
            out.write(",");
            out.write(CRLF);
        }
    }

    @Override
    public JsonGenerator writeKey(String name) {
        try {
            JSONString jname = new JSONString(name);
            out.write(jname.toString());
            out.write(": ");
            return this;
        } catch (IOException ex) {
            Logger.getLogger(JSONPGenerator.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException();
        }
    }

    @Override
    public JsonGenerator writeStartArray() {
        try {
            writeSeparator();
            out.write("[");
            closeCode.add("]");
            return new JSONPGenerator(this, out);
        } catch (IOException ex) {
            Logger.getLogger(JSONPGenerator.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException();
        }
    }

    @Override
    public JsonGenerator writeStartArray(String name) {
        try {
            writeSeparator();
            writeKey(name);
            out.write("[");
            closeCode.add("]");
            return new JSONPGenerator(this, out);
        } catch (IOException ex) {
            Logger.getLogger(JSONPGenerator.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException();
        }
    }

    @Override
    public JsonGenerator write(String name, JsonValue value) {
        write(name, JSON.valueOf(value));
        return this;
    }

    void write(String name, JSONValue value) {
        try {
            writeSeparator();
            writeKey(name);
            out.write(value.toString());
        } catch (IOException ex) {
            Logger.getLogger(JSONPGenerator.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Override
    public JsonGenerator write(String name, String value) {
        write(name, (JSONValue) new JSONString(value));
        return this;
    }

    @Override
    public JsonGenerator write(String name, BigInteger value) {
        write(name, (JSONValue) new JSONNumber(value));
        return this;
    }

    @Override
    public JsonGenerator write(String name, BigDecimal value) {
        write(name, (JSONValue) new JSONNumber(value));
        return this;
    }

    @Override
    public JsonGenerator write(String name, int value) {
        write(name, (JSONValue) new JSONNumber(value));
        return this;
    }

    @Override
    public JsonGenerator write(String name, long value) {
        write(name, (JSONValue) new JSONNumber(value));
        return this;
    }

    @Override
    public JsonGenerator write(String name, double value) {
        write(name, (JSONValue) new JSONNumber(value));
        return this;
    }

    @Override
    public JsonGenerator write(String name, boolean value) {
        write(name, (JSONValue) (value ? JSONBoolean.TRUE : JSONBoolean.FALSE));
        return this;
    }

    @Override
    public JsonGenerator writeNull(String name) {
        write(name, (JSONValue) JSONNULL.NULL);
        return this;
    }

    @Override
    public JsonGenerator writeEnd() {
        try {
            String end = up.closeCode.get(0);
            out.write(end);
            return up;
        } catch (IOException ex) {
            Logger.getLogger(JSONPGenerator.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException();
        }
    }

    @Override
    public JsonGenerator write(JsonValue value) {
        write(JSON.valueOf(value));
        return this;
    }

    void write(JSONValue value) {
        try {
            out.write(value.toString());
        } catch (IOException ex) {
            Logger.getLogger(JSONPGenerator.class.getName()).log(Level.SEVERE, null, ex);
            throw new UnsupportedOperationException();
        }
    }

    @Override
    public JsonGenerator write(String value) {
        write((JSONValue) new JSONString(value));
        return this;
    }

    @Override
    public JsonGenerator write(BigDecimal value) {
        write((JSONValue) new JSONNumber(value));
        return this;
    }

    @Override
    public JsonGenerator write(BigInteger value) {
        write((JSONValue) new JSONNumber(value));
        return this;
    }

    @Override
    public JsonGenerator write(int value) {
        write((JSONValue) new JSONNumber(value));
        return this;
    }

    @Override
    public JsonGenerator write(long value) {
        write((JSONValue) new JSONNumber(value));
        return this;
    }

    @Override
    public JsonGenerator write(double value) {
        write((JSONValue) new JSONNumber(value));
        return this;
    }

    @Override
    public JsonGenerator write(boolean value) {
        write((JSONValue) (value ? JSONBoolean.TRUE : JSONBoolean.FALSE));
        return this;
    }

    @Override
    public JsonGenerator writeNull() {
        write((JSONValue) JSONNULL.NULL);
        return this;
    }

    @Override
    public void close() {
        try {
            out.close();
        } catch (IOException ex) {
            Logger.getLogger(JSONPGenerator.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

    @Override
    public void flush() {
        try {
            out.flush();
        } catch (IOException ex) {
            Logger.getLogger(JSONPGenerator.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

}
