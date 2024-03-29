package net.siisise.json.spi;

import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Collection;
import java.util.Map;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonBuilderFactory;
import javax.json.JsonMergePatch;
import javax.json.JsonNumber;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonPatch;
import javax.json.JsonPatchBuilder;
import javax.json.JsonPointer;
import javax.json.JsonReader;
import javax.json.JsonReaderFactory;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import javax.json.spi.JsonProvider;
import javax.json.stream.JsonGenerator;
import javax.json.stream.JsonGeneratorFactory;
import javax.json.stream.JsonParser;
import javax.json.stream.JsonParserFactory;
import net.siisise.json.JSONMergePatch7396;
import net.siisise.json.JSONPatch;
import net.siisise.json.JSONPatchBuilder;
import net.siisise.json.JSONPointer;
import net.siisise.json.jsonp.JSONPBuilderFactory;
import net.siisise.json.jsonp.JSONPReaderFactory;
import net.siisise.json.jsonp.JSONPWriterFactory;
import net.siisise.json.stream.JSONPGeneratorFactory;
import net.siisise.json.stream.JSONPParserFactory;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONString;
import net.siisise.json.JSON;
import net.siisise.json.JSONValue;

/**
 * JSR 374 Java API for JSON Processing (JSON-P)
 */
public class JSONPProvider extends JsonProvider {

    private JsonParserFactory pf;
    private JsonGeneratorFactory gf;
    private JsonWriterFactory wf;
    private JsonReaderFactory rf;
    private JsonBuilderFactory bf;

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
    public JsonGeneratorFactory createGeneratorFactory(Map<String, ?> config) {
        return gf = new JSONPGeneratorFactory(config);
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
    public JsonWriterFactory createWriterFactory(Map<String, ?> config) {
        return wf = new JSONPWriterFactory(config);
    }

    @Override
    public JsonReaderFactory createReaderFactory(Map<String, ?> config) {
        return rf = new JSONPReaderFactory(config);
    }

    @Override
    public JsonObjectBuilder createObjectBuilder() {
        if ( bf == null ) { createBuilderFactory(null); }
        return bf.createObjectBuilder();
    }

    @Override
    public JsonObjectBuilder createObjectBuilder(JsonObject object) {
        if ( bf == null ) { createBuilderFactory(null); }
        return bf.createObjectBuilder(object);
    }

    /**
     *
     * @param object
     * @return
     */
    @Override
    public JsonObjectBuilder createObjectBuilder(Map<String, Object> object) {
        if ( bf == null ) { createBuilderFactory(null); }
        return bf.createObjectBuilder((Map<String,Object>)object);
    }

    @Override
    public JsonArrayBuilder createArrayBuilder() {
        if ( bf == null ) { createBuilderFactory(null); }
        return bf.createArrayBuilder();
    }
    
    @Override
    public JsonArrayBuilder createArrayBuilder(JsonArray array) {
        if ( bf == null ) { createBuilderFactory(null); }
        return bf.createArrayBuilder(array);
    }

    @Override
    public JsonPointer createPointer(String jsonPointer) {
        return new JSONPointer(jsonPointer);
    }

    /**
     * JSON Patch
     * @return 
     */
    @Override
    public JsonPatchBuilder createPatchBuilder() {
        return new JSONPatchBuilder();
    }

    /**
     * JSON Patch
     * @param array
     * @return 
     */
    @Override
    public JsonPatchBuilder createPatchBuilder(JsonArray array) {
        return new JSONPatchBuilder(array);
    }
    
    /**
     * RFC 6902
     * @param array
     * @return 
     */
    @Override
    public JsonPatch createPatch(JsonArray array) {
//        return new JSONPatch(array);
        return createPatchBuilder(array).build();
    }
    
    /**
     * RFC 6902
     * @param source
     * @param target
     * @return 
     */
    @Override
    public JsonPatch createDiff(JsonStructure source, JsonStructure target) {
        JSONValue s = JSON.valueOf(source);
        JSONValue t = JSON.valueOf(target);
        return JSONPatch.diff(s, t);
    }

    @Override
    public JsonMergePatch createMergePatch(JsonValue patch) {
        return new JSONMergePatch7396(JSON.valueOf(patch));
    }

    @Override
    public JsonMergePatch createMergeDiff(JsonValue source, JsonValue target) {
        JSONValue s = JSON.valueOf(source);
        JSONValue t = JSON.valueOf(target);
        return new JSONMergePatch7396(JSONMergePatch7396.diff(s, t));
    }

    @Override
    public JsonArrayBuilder createArrayBuilder(Collection<?> collection) {
        if ( bf == null ) { createBuilderFactory(null); }
        return bf.createArrayBuilder(collection);
    }

    @Override
    public JsonBuilderFactory createBuilderFactory(Map<String, ?> config) {
        return bf = new JSONPBuilderFactory(config);
    }

    @Override
    public JSONString createValue(String value) {
        return new JSONString(value);
    }

    @Override
    public JSONNumber createValue(int value) {
        return new JSONNumber(value);
    }

    @Override
    public JSONNumber createValue(long value) {
        return new JSONNumber(value);
    }

    @Override
    public JsonNumber createValue(double value) {
        return new JSONNumber(value);
    }

    @Override
    public JsonNumber createValue(BigDecimal value) {
        return new JSONNumber(value);
    }

    @Override
    public JsonNumber createValue(BigInteger value) {
        return new JSONNumber(value);
    }
}
