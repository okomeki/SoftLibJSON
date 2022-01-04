package net.siisise.json2.jsonp.stream;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Stream;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;
import javax.json.stream.JsonLocation;
import javax.json.stream.JsonParser;
import javax.json.stream.JsonParsingException;
import net.siisise.io.FrontPacket;
import net.siisise.io.Packet;
import net.siisise.io.StreamFrontPacket;
import net.siisise.json2.JSON2;
import net.siisise.json2.JSON28259Reg;
import net.siisise.json2.JSON2Array;
import net.siisise.json2.JSON2Boolean;
import net.siisise.json2.JSON2NULL;
import net.siisise.json2.JSON2Number;
import net.siisise.json2.JSON2Object;
import net.siisise.json2.JSON2String;
import net.siisise.json2.JSON2Value;

/**
 * ABNF Parserを使っているのでこちらは軽く実装.
 */
public class SLJsonParser implements JsonParser {

    static class Next {
        Object json;
        Event state;

        Next(Object json, Event state) {
            this.json = json;
            this.state = state;
        }
    }

    FrontPacket stream;
    
    List<Next> nexts = new ArrayList<>();
    
    public SLJsonParser(Reader reader) {
        stream = new StreamFrontPacket(reader);
    }

    public SLJsonParser(InputStream reader) {
        stream = new StreamFrontPacket(reader);
    }
    
    public SLJsonParser(Object json) {
        nexts = nexts(json);
    }
    
    static List<Next> nexts(Object src) {
        JSON2Value json = JSON2.valueOf(src);
        List<Next> nexts = new ArrayList<>();
        if ( json instanceof JSON2Object ) {
            nexts.add(new Next(null, Event.START_OBJECT));
            for ( String key : ((JSON2Object<?>) json).keySet() ) {
                nexts.add(new Next(key, Event.KEY_NAME));
                nexts.addAll(nexts(((JSON2Object)json).get(key)));
            }
            nexts.add(new Next(json, Event.END_OBJECT));
        } else if ( json instanceof JSON2Array ) {
            nexts.add(new Next(null, Event.START_ARRAY));
            for ( Object val : ((JSON2Array<?>) json)) {
                nexts.addAll(nexts(val));
            }
            nexts.add(new Next(json, Event.END_ARRAY));
        } else if ( json instanceof JSON2String ) {
            nexts.add(new Next(json, Event.VALUE_STRING));
        } else if ( json instanceof JSON2Number ) {
            nexts.add(new Next(json, Event.VALUE_NUMBER));
        } else if ( json instanceof JSON2Boolean ) {
            nexts.add(new Next(json, ((boolean)(((JSON2Boolean)json).map())) ? Event.VALUE_TRUE : Event.VALUE_FALSE ));
        } else if ( json instanceof JSON2NULL ) {
            nexts.add(new Next(json, Event.VALUE_NULL));
        }
        return nexts;
    }
    
    /**
     * OBJECT, ARRAYは再構築する
     * @param step nexts 
     * @return 
     */
    JSON2Value parseValue() {
        JSON2Value val = null;
        switch (current.state) {
            case START_OBJECT:
                val = parseObject();
                break;
            case START_ARRAY:
                val = parseArray();
                break;
            case VALUE_STRING:
            case VALUE_NUMBER:
            case VALUE_TRUE:
            case VALUE_FALSE:
            case VALUE_NULL:
                val = (JSON2Value) current.json;
                break;
            default:
                throw new IllegalStateException();
        }
        return val;
    }

    /**
     * 
     * @param step nexts START_OBJECT の次から
     * @return 
     */
    JSON2Object parseObject() {
        JSON2Object obj = new JSON2Object();
        next();
        while ( current.state != Event.END_OBJECT ) {
            if ( current.state == Event.KEY_NAME ) {
                String name = ((String)current.json);
                next();
                JSON2Value val = parseValue();
                obj.addJSON(name, val);
            } else {
                throw new IllegalStateException();
            }
            next();
        }
        return obj;
    }

    /**
     * 
     * @return 
     */
    JSON2Array parseArray() {
        JSON2Array obj = new JSON2Array();
        next();
        while ( current.state != Event.END_ARRAY ) {
            JSON2Value val = parseValue();
            obj.addJSON("-",val);
            next();
        }
        return obj;
    }

    @Override
    public boolean hasNext() throws JsonParsingException {
        if (nexts.isEmpty() && stream.size() > 0) {
            Packet p;
            Next nextb;
            //nexts = nexts(JSON2.parseWrap(stream));
            if ( JSON28259Reg.value_separator.is(stream) != null ) {
                if ( current == null || current.state == Event.START_ARRAY || current.state == Event.START_OBJECT || current.state == Event.KEY_NAME ) {
                    // エラーにするといい
                    throw new JsonParsingException("カンマな位置が", getLocation());
                } else {
                    // 読み飛ばす
                }
            }
            if ( JSON28259Reg.begin_array.is(stream) != null ) {
                nextb = new Next(null, Event.START_ARRAY);
            } else if ( JSON28259Reg.begin_object.is(stream) != null ) {
                nextb = new Next(null, Event.START_OBJECT);
            } else if ( JSON28259Reg.end_array.is(stream) != null ) {
                nextb = new Next(null, Event.END_ARRAY);
            } else if ( JSON28259Reg.end_object.is(stream) != null ) {
                nextb = new Next(null, Event.END_OBJECT);
            } else if ( (p = JSON28259Reg.string.pl(JSON28259Reg.name_separator).is(stream)) != null ) {
                nextb = new Next(JSON28259Reg.parse("string", p), Event.KEY_NAME);
            } else if ( JSON28259Reg.FALSE.is(stream) != null ) {
                nextb = new Next(JSON2Boolean.FALSE, Event.VALUE_FALSE);
            } else if ( JSON28259Reg.TRUE.is(stream) != null ) {
                nextb = new Next(JSON2Boolean.TRUE, Event.VALUE_TRUE);
            } else if ( JSON28259Reg.NULL.is(stream) != null ) {
                nextb = new Next(JSON2NULL.NULL, Event.VALUE_NULL);
            } else if ( (p = JSON28259Reg.number.is(stream)) != null ) {
                nextb = new Next(new JSON2Number(JSON28259Reg.parse("number", p)), Event.VALUE_NUMBER);
            } else if ( (p = JSON28259Reg.string.is(stream)) != null ) {
                nextb = new Next(new JSON2String(JSON28259Reg.parse("string", p)), Event.VALUE_STRING);
            } else {
                throw new JsonParsingException("不明な構文", getLocation());
            }
            nexts.add(nextb);
        }
        return !nexts.isEmpty();
    }
    
    Next current;

    @Override
    public Event next() {
        if (!hasNext()) {
            current = null;
            throw new NoSuchElementException();
        }
        current = nexts.get(0);
        nexts.remove(0);
        return current.state;
    }

    @Override
    public String getString() {
        return current.json.toString();
    }

    @Override
    public boolean isIntegralNumber() {
        if ( current.json instanceof JSON2Number ) {
            return ((JSON2Number)current.json).isIntegral();
        }
        return false;
    }

    @Override
    public int getInt() {
        if ( current.state == Event.VALUE_NUMBER ) {
            return ((JSON2Number)current.json).intValue();
        }
        throw new IllegalStateException();
//        return Integer.parseInt(current.json.toString());
    }

    @Override
    public long getLong() {
        if ( current.state == Event.VALUE_NUMBER ) {
            return ((JSON2Number)current.json).longValue();
        }
        throw new IllegalStateException();
//        return Long.parseLong(current.json.toString());
    }

    @Override
    public BigDecimal getBigDecimal() {
        if ( current.state == Event.VALUE_NUMBER ) {
            return ((JSON2Number)current.json).bigDecimalValue();
        }
        throw new IllegalStateException();
//        return new BigDecimal(current.json.toString());
    }

    @Override
    public JsonLocation getLocation() {
        return new JsonLocation() {
            @Override
            public long getLineNumber() {
                return -1l;
            }

            @Override
            public long getColumnNumber() {
                return -1l;
            }

            @Override
            public long getStreamOffset() {
                return -1l;
            }
        };
    }
    
    @Override
    public JsonObject getObject() {
        if ( current.state == Event.START_OBJECT ) {
            return parseObject().toJson();
        }
        throw new IllegalStateException();
    }

    @Override
    public JsonArray getArray() {
        if ( current.state == Event.START_ARRAY ) {
            return parseArray().toJson();
        }
        throw new IllegalStateException();
    }
    
    @Override
    public JsonValue getValue() {
        return parseValue().toJson();
    }
    
    @Override
    public Stream<JsonValue> getArrayStream() {
        return getArray().stream();
    }

    @Override
    public Stream<Map.Entry<String,JsonValue>> getObjectStream() {
        return getObject().entrySet().stream();
    }

    /**
     * なにをするもの?
     * @return 
     */
    @Override
    public Stream<JsonValue> getValueStream() {
        List<JsonValue> values = new ArrayList<>();
        values.add(getValue());
        while ( hasNext() ) {
            next();
            values.add(getValue());
        }
        return values.stream();
    }

    @Override
    public void close() {
        if ( stream != null ) {
            try {
                stream.getInputStream().close();
            } catch (IOException ex) {
                Logger.getLogger(SLJsonParser.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }
    
}
