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
import javax.json.JsonString;
import javax.json.JsonStructure;
import javax.json.JsonValue;
import javax.json.JsonWriter;
import javax.json.JsonWriterFactory;
import javax.json.spi.JsonProvider;
import javax.json.stream.JsonGenerator;
import javax.json.stream.JsonGeneratorFactory;
import javax.json.stream.JsonParser;
import javax.json.stream.JsonParserFactory;
import net.siisise.json.mergepatch.JSONMergePatch7396;
import net.siisise.json.pointer.JSONPatchBuilder;
import net.siisise.json.pointer.JSONPointer;
import net.siisise.json2.jsonp.JSONPBuilderFactory;
import net.siisise.json2.jsonp.JSONPReaderFactory;
import net.siisise.json2.jsonp.JSONPWriterFactory;
import net.siisise.json.stream.JSONPGeneratorFactory;
import net.siisise.json.stream.JSONPParserFactory;
import net.siisise.json2.JSON2;
import net.siisise.json2.JSON2Array;
import net.siisise.json2.JSON2Number;
import net.siisise.json2.JSON2String;
import net.siisise.json2.JSON2Value;

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
        return bf.createObjectBuilder(object);
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
     */
    @Override
    public JsonPatchBuilder createPatchBuilder() {
        return new JSONPatchBuilder();
    }

    /**
     * JSON Patch
     */
    @Override
    public JsonPatchBuilder createPatchBuilder(JsonArray array) {
        return new JSONPatchBuilder((JSON2Array) JSON2.valueOf(array));
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
        throw new UnsupportedOperationException();
    }

    @Override
    public JsonMergePatch createMergePatch(JsonValue patch) {
        return new JSONMergePatch7396(JSON2.valueOf(patch));
    }

    @Override
    public JsonMergePatch createMergeDiff(JsonValue source, JsonValue target) {
        JSON2Value s = JSON2.valueOf(source);
        JSON2Value t = JSON2.valueOf(target);
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
    public JsonString createValue(String value) {
        return new JSON2String(value);
    }

    @Override
    public JsonNumber createValue(int value) {
        return new JSON2Number(value);
    }

    @Override
    public JsonNumber createValue(long value) {
        return new JSON2Number(value);
    }

    @Override
    public JsonNumber createValue(double value) {
        return new JSON2Number(value);
    }

    @Override
    public JsonNumber createValue(BigDecimal value) {
        return new JSON2Number(value);
    }

    @Override
    public JsonNumber createValue(BigInteger value) {
        return new JSON2Number(value);
    }
}
