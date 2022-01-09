package net.siisise.json.spi;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.util.Map;
import javax.json.JsonArrayBuilder;
import javax.json.JsonBuilderFactory;
import javax.json.JsonObjectBuilder;
import javax.json.JsonPatchBuilder;
import javax.json.JsonPointer;
import javax.json.JsonReader;
import javax.json.JsonReaderFactory;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import javax.json.spi.JsonProvider;
import javax.json.stream.JsonGenerator;
import javax.json.stream.JsonGeneratorFactory;
import javax.json.stream.JsonParser;
import javax.json.stream.JsonParserFactory;
import net.siisise.json.pointer.JSONPatchBuilder;
import net.siisise.json.pointer.JSONPointer;
import net.siisise.json2.jsonp.JSONPBuilderFactory;
import net.siisise.json2.jsonp.JSONPReaderFactory;
import net.siisise.json2.jsonp.JSONPWriterFactory;
import net.siisise.json.stream.JSONPGeneratorFactory;
import net.siisise.json.stream.JSONPParserFactory;

/**
 * JSR 374 Java API for JSON Processing (JSON-P)
 */
public class JSONPProvider extends JsonProvider {

    JsonParserFactory pf;
    JsonGeneratorFactory gf;
    JsonWriterFactory wf;
    JsonReaderFactory rf;
    JsonBuilderFactory bf;

    @Override
    public JsonParser createParser(Reader reader) {
        if ( pf == null ) { createParserFactory(null); }
        return pf.createParser(reader);
    }

    @Override
    public JsonParser createParser(InputStream in) {
        if ( pf == null ) { createParserFactory(null); }
        return pf.createParser(in);
    }

    @Override
    public JsonParserFactory createParserFactory(Map<String, ?> map) {
        return pf = new JSONPParserFactory();
    }

    @Override
    public JsonGenerator createGenerator(Writer writer) {
        if ( gf == null ) {createGeneratorFactory(null); }
        return gf.createGenerator(writer);
    }

    @Override
    public JsonGenerator createGenerator(OutputStream out) {
        if ( gf == null ) {createGeneratorFactory(null); }
        return gf.createGenerator(out);
    }

    @Override
    public JsonGeneratorFactory createGeneratorFactory(Map<String, ?> map) {
        return gf = new JSONPGeneratorFactory();
    }

    @Override
    public JsonReader createReader(Reader reader) {
        if ( rf == null ) { createReaderFactory(null); }
        return rf.createReader(reader);
    }

    @Override
    public JsonReader createReader(InputStream in) {
        if ( rf == null ) { createReaderFactory(null); }
        return rf.createReader(in);
    }

    @Override
    public JsonWriter createWriter(Writer writer) {
        if ( wf == null ) { createWriterFactory(null); }
        return wf.createWriter(writer);
    }

    @Override
    public JsonWriter createWriter(OutputStream out) {
        if ( wf == null ) { createWriterFactory(null); }
        return wf.createWriter(out);
    }
    
    @Override
    public JsonWriterFactory createWriterFactory(Map<String, ?> map) {
        return wf = new JSONPWriterFactory(map);
    }

    @Override
    public JsonReaderFactory createReaderFactory(Map<String, ?> map) {
        return rf = new JSONPReaderFactory(map);
    }

    @Override
    public JsonObjectBuilder createObjectBuilder() {
        if ( bf == null ) { createBuilderFactory(null); }
        return bf.createObjectBuilder();
    }

    @Override
    public JsonArrayBuilder createArrayBuilder() {
        if ( bf == null ) { createBuilderFactory(null); }
        return bf.createArrayBuilder();
    }
    
    @Override
    public JsonBuilderFactory createBuilderFactory(Map<String, ?> map) {
        return bf = new JSONPBuilderFactory();
    }

    @Override
    public JsonPointer createPointer(String jsonPointer) {
        return new JSONPointer(jsonPointer);
    }
    
    /**
     * JSON Patch
     */
    @Override
    public JsonPatchBuilder createPatchBuilder() {
        return new JSONPatchBuilder();
    }
}
