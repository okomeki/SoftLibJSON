package net.siisise.json.stream;

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
import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONString;
import net.siisise.json.JSON;
import net.siisise.json.JSONValue;
import net.siisise.json.base.JSONBaseNULL;
import net.siisise.json.bind.target.JSONFormat;

/**
 * タブ暫定対応
 */
public class JSONPGenerator implements JsonGenerator {

    private final Writer out;
    private final JSONFormat format;

    boolean first = true;

    int tabsize = 0;

    List<String> closeCode = new ArrayList();

    JSONPGenerator(Writer writer, JSONFormat f) {
        out = writer;
        format = f;
    }

    @Override
    public JsonGenerator writeStartObject() {
        try {
            writeSeparator();
            tabin("{","}");
            return this;
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public JsonGenerator writeStartObject(String name) {
        try {
            writeSeparator();
            writeKey(name);
            tabin("{","}");
            return this;
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

    void writeSeparator() throws IOException {
        if (first) {
            first = false;
        } else {
            out.write(",");
            out.write(format.crlf);
        }
    }

    @Override
    public JsonGenerator writeKey(String name) {
        try {
            JSONString jname = new JSONString(name);
            tab(jname.toJSON());
            out.write(": ");
            return this;
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public JsonGenerator writeStartArray() {
        try {
            writeSeparator();
            tabin("[","]");
            return this;
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public JsonGenerator writeStartArray(String name) {
        try {
            writeSeparator();
            writeKey(name);
            tabin("[","]");
            return this;
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
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
            tab(value.toJSON(format));
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
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
        write(name, (JSONValue) JSONBoolean.valieOf(value));
        return this;
    }

    @Override
    public JsonGenerator writeNull(String name) {
        write(name, (JSONValue) JSONBaseNULL.NULL);
        return this;
    }

    private void tab(String txt) throws IOException {
        StringBuilder tabs = new StringBuilder(100);
        for ( int i = 0; i < tabsize; i++ ) {
            tabs.append(format.tab);
        }
        String src = tabs.toString() + txt.replace("\r\n", format.crlf + tabs.toString());
        out.write(src);
        
    }
    
    private void tabln(String txt) throws IOException {
        tab(txt);
        out.write(format.crlf);
    }
    
    private void tabin(String start, String end) throws IOException {
        tabln(start);
        tabsize++;
        closeCode.add(0,end);
        first = true;
    }
    
    private void tabout() throws IOException {
        String end = closeCode.get(0);
        closeCode.remove(0);
        tabsize--;
        tabln(end);
    }

    @Override
    public JsonGenerator writeEnd() {
        try {
            tabout();
            return this;
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public JsonGenerator write(JsonValue value) {
        write(JSON.valueOf(value));
        return this;
    }

    void write(JSONValue value) {
        try {
            tab(value.toJSON(format));
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
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
        write((JSONValue) JSONBoolean.valieOf(value));
        return this;
    }

    @Override
    public JsonGenerator writeNull() {
        write((JSONValue) JSONBaseNULL.NULL);
        return this;
    }

    @Override
    public void close() {
        try {
            out.close();
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

    @Override
    public void flush() {
        try {
            out.flush();
        } catch (IOException ex) {
            throw new IllegalStateException(ex);
        }
    }

}
