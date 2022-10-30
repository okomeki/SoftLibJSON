package net.siisise.json.bind.target;

import java.util.Collection;
import java.util.Map;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONObject;
import net.siisise.json.JSONString;
import net.siisise.json.bind.OMAP;
import net.siisise.json.JSONValue;
import net.siisise.json.base.JSONBaseNULL;

/**
 * List,Mapは表面をJSON2系にラップするだけと、中もprimitive系に更新するのと2種類想定するかもしれない
 */
public class JSONConvert extends OBJConvert<JSONValue> {

    @Override
    public Class<JSONValue> targetClass() {
        return JSONValue.class;
    }

    @Override
    public JSONBaseNULL nullValue() {
        return JSONBaseNULL.NULL;
    }

    @Override
    public JSONBoolean booleanValue(Boolean bool) {
        return bool ? JSONBoolean.TRUE : JSONBoolean.FALSE;
    }

    @Override
    public JSONNumber numberValue(Number num) {
        return new JSONNumber(num);
    }

    @Override
    public JSONString stringValue(CharSequence str) {
        return new JSONString(str);
    }

    /**
     * 表面のみ変換
     * @param list
     * @return 
     */
    @Override
    public JSONArray listValue(Collection list) {
        if ( list instanceof JSONArray ) {
            return (JSONArray) list;
        }
        return new JSONArray(list);
    }

    /**
     * 表面のみ変換
     * @param map
     * @return 
     */
    @Override
    public JSONObject mapValue(Map map) {
        if ( map instanceof JSONObject ) {
            return (JSONObject) map;
        }
        JSONObject obj = new JSONObject();
        ((Map<Object,Object>)map).forEach((k,v) -> {
            obj.put(k.toString(), v);
        });
        return obj;
    }

    /**
     * 表面のみ変換
     * @param obj
     * @return 
     */
    @Override
    public JSONValue objectValue(Object obj) {
        // toJSON メソッドで変換
        JSONValue json = OMAP.toJSON(obj);
        if ( json != null ) {
            return json;
        }
        return super.objectValue(obj);
    }
}
