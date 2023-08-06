package net.siisise.json.stream;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.stream.Stream;
import javax.json.JsonArray;
import javax.json.JsonObject;
import javax.json.JsonValue;
import javax.json.stream.JsonLocation;
import javax.json.stream.JsonParser;
import javax.json.stream.JsonParsingException;
import net.siisise.abnf.ABNF;
import net.siisise.bind.Rebind;
import net.siisise.block.ReadableBlock;
import net.siisise.json.parser.JSON8259Reg;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONObject;
import net.siisise.json.bind.OMAP;

/**
 * ABNF Parserを使っているのでこちらは軽く実装.
 */
public class JSONPParser implements JsonParser {

    static private class Next {
        Object json;
        Event state;

        Next(Object json, Event state) {
            this.json = json;
            this.state = state;
        }
    }

    private ReadableBlock stream; 
    /**
     * object をばらしたときは全部格納するのでList
     */
    private List<Next> nexts = new ArrayList<>();
    
    public JSONPParser(Reader reader) {
        stream = ReadableBlock.wrap(reader);
    }

    public JSONPParser(InputStream reader) {
        stream = ReadableBlock.wrap(reader);
    }
    
    public JSONPParser(Object json) {
        nexts = nexts(json);
    }

    /**
     * object の分解
     * @param src
     * @return 
     */
    static List<Next> nexts(Object src) {
        Object obj = OMAP.valueOf(src, Object.class);
        List<Next> nexts = new ArrayList<>();
        if ( obj == null ) {
            nexts.add(new Next(obj, Event.VALUE_NULL));
        } else if ( obj instanceof Map ) {
            nexts.add(new Next(null, Event.START_OBJECT));
            for ( String key : ((Map<String,?>) obj).keySet() ) {
                nexts.add(new Next(key, Event.KEY_NAME));
                nexts.addAll(nexts(((Map)obj).get(key)));
            }
            nexts.add(new Next(obj, Event.END_OBJECT));
        } else if ( obj instanceof List ) {
            nexts.add(new Next(null, Event.START_ARRAY));
            for ( Object val : ((List<?>) obj)) {
                nexts.addAll(nexts(val));
            }
            nexts.add(new Next(obj, Event.END_ARRAY));
        } else if ( obj instanceof String ) {
            nexts.add(new Next(obj, Event.VALUE_STRING));
        } else if ( obj instanceof Number ) {
            nexts.add(new Next(obj, Event.VALUE_NUMBER));
        } else if ( obj instanceof Boolean ) {
            nexts.add(new Next(obj, ((Boolean)obj) ? Event.VALUE_TRUE : Event.VALUE_FALSE ));
        }
        return nexts;
    }

    private Next current;

    /**
     * OBJECT, ARRAYは再構築する
     * @param step nexts 
     * @return 
     */
    private Object parseValue() {
        Object val = null;
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
                val = current.json;
                break;
            default:
                throw new IllegalStateException();
        }
        return val;
    }

    /**
     * parse object. 
     * @param step nexts START_OBJECT の次から
     * @return 
     */
    private Map parseObject() {
        JSONObject obj = new JSONObject();
        next();
        while ( current.state != Event.END_OBJECT ) {
            if ( current.state == Event.KEY_NAME ) {
                String name = ((String)current.json);
                next();
                Object val = parseValue();
                obj.put(name, val);
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
    private List parseArray() {
        JSONArray obj = new JSONArray();
        next();
        while ( current.state != Event.END_ARRAY ) {
            Object val = parseValue();
            obj.add(val);
            next();
        }
        return obj;
    }
    
    static ABNF stringsp = JSON8259Reg.string.pl(JSON8259Reg.name_separator);

    /**
     * streamからはそれっぽくパースするがステートレスなので全体的な正しさは保証しない。
     * カンマ区切りが苦手.
     * 数値、文字列などは別でparseしているので is のみで完結しているわけではない。
     * @return 次のデータがある
     * @throws JsonParsingException 
     */
    @Override
    public boolean hasNext() throws JsonParsingException {
        if (nexts.isEmpty() && stream != null && stream.size() > 0) {
            ReadableBlock p;
            Next nextb = null;
            if ( JSON8259Reg.value_separator.is(stream) != null ) { // JsonParserでは扱っていないので変な位置にでてこないようチェック
                if ( current == null
                    || current.state == Event.START_ARRAY
                    || current.state == Event.START_OBJECT
                    || current.state == Event.KEY_NAME ) {
                    // エラーにするといい
                    throw new JsonParsingException("カンマな位置が", getLocation());
                } else {
                    // 読み飛ばす
                    nextb = new Next(',',null);
                }
            }
            if ( JSON8259Reg.begin_array.is(stream) != null ) {
                nextb = new Next(null, Event.START_ARRAY);
            } else if ( JSON8259Reg.begin_object.is(stream) != null ) {
                nextb = new Next(null, Event.START_OBJECT);
            } else if ( JSON8259Reg.end_array.is(stream) != null ) {
                if ( nextb != null ) {
                    throw new JsonParsingException("カンマな位置が", getLocation());
                }
                nextb = new Next(null, Event.END_ARRAY);
            } else if ( JSON8259Reg.end_object.is(stream) != null ) {
                if ( nextb != null ) {
                    throw new JsonParsingException("カンマな位置が", getLocation());
                }
                nextb = new Next(null, Event.END_OBJECT);
            } else if ( (p = stringsp.is(stream)) != null ) {
                nextb = new Next(JSON8259Reg.parse("string", p), Event.KEY_NAME);
            } else if ( JSON8259Reg.FALSE.is(stream) != null ) {
                nextb = new Next(false, Event.VALUE_FALSE);
            } else if ( JSON8259Reg.TRUE.is(stream) != null ) {
                nextb = new Next(true, Event.VALUE_TRUE);
            } else if ( JSON8259Reg.NULL.is(stream) != null ) {
                nextb = new Next(null, Event.VALUE_NULL);
            } else if ( (p = JSON8259Reg.number.is(stream)) != null ) {
                nextb = new Next(JSON8259Reg.parse("number", p), Event.VALUE_NUMBER);
            } else if ( (p = JSON8259Reg.string.is(stream)) != null ) {
                nextb = new Next(JSON8259Reg.parse("string", p), Event.VALUE_STRING);
            } else {
                return false;
            }
            nexts.add(nextb);
        }
        return !nexts.isEmpty();
    }
    
    /**
     * 次のEvent
     * @return 次のEvent
     */
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

    /**
     * 
     * @return 
     */
    @Override
    public String getString() {
        if ( current.state == Event.KEY_NAME || current.state == Event.VALUE_STRING || current.state == Event.VALUE_NUMBER ) {
            return current.json.toString();
        } else {
            throw new IllegalStateException();
        }
    }

    @Override
    public boolean isIntegralNumber() {
        if ( current.json instanceof Number ) {
            return new JSONNumber((Number)current.json).isIntegral();
        }
        return false;
    }

    @Override
    public int getInt() {
        if ( current.state == Event.VALUE_NUMBER ) {
            return ((Number)current.json).intValue();
        }
        throw new IllegalStateException();
//        return Integer.parseInt(current.json.toJSON());
    }

    @Override
    public long getLong() {
        if ( current.state == Event.VALUE_NUMBER ) {
            return ((Number)current.json).longValue();
        }
        throw new IllegalStateException();
//        return Long.parseLong(current.json.toJSON());
    }

    @Override
    public BigDecimal getBigDecimal() {
        if ( current.state == Event.VALUE_NUMBER ) {
            return new JSONNumber((Number)current.json).bigDecimalValue();
        }
        throw new IllegalStateException();
//        return new BigDecimal(current.json.toJSON());
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
                return stream != null ? stream.backSize() : -1;
            }
        };
    }
    
    @Override
    public JsonObject getObject() {
        if ( current.state == Event.START_OBJECT ) {
            return Rebind.valueOf(parseObject(), JsonValue.class);
        }
        throw new IllegalStateException();
    }

    @Override
    public JsonArray getArray() {
        if ( current.state == Event.START_ARRAY ) {
            return Rebind.valueOf(parseArray(), JsonArray.class);
        }
        throw new IllegalStateException();
    }
    
    @Override
    public JsonValue getValue() {
        return OMAP.valueOf(parseValue(), JsonValue.class);
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
     * 外側が複数ある場合とArrayの中でも使えるっぽくしておく
     * @return 
     */
    @Override
    public Stream<JsonValue> getValueStream() {
        List<JsonValue> values = new ArrayList<>();
        values.add(getValue());
        while ( hasNext() ) {
            Event ev = next();
            if ( ev == Event.END_ARRAY ||
                    ev == Event.END_OBJECT ||
                    ev == Event.KEY_NAME ) {
                break;
            }
            values.add(getValue());
        }
        return values.stream();
    }

    @Override
    public void close() {
        if ( stream != null ) {
            try {
                stream.getInputStream().close();
                stream = null;
            } catch (IOException ex) {
                throw new IllegalStateException(ex);
            }
        }
    }
    
}
