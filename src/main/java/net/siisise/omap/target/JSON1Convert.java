package net.siisise.omap.target;

import java.util.Collection;
import java.util.Map;
import net.siisise.json.JSON;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONNULL;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONObject;
import net.siisise.json.JSONString;
import net.siisise.json.JSONValue;
import net.siisise.omap.OMAP;

/**
 *
 */
public class JSON1Convert extends OBJConvert<JSONValue> {

    @Override
    public Class<JSONValue> targetClass() {
        return JSONValue.class;
    }
    
    @Override
    public JSONNULL nullValue() {
        return JSONNULL.NULL;
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
        return new JSONString(str.toString());
    }

    @Override
    public JSONArray listValue(Collection list) {
        return (JSONArray) list.stream().collect(JSON.toJSONArray());
    }

    @Override
    public JSONObject mapValue(Map map) {
        return new JSONObject(map);
    }

    /**
     * Java Objectの公開フィールドを取得してJSONObjectに変換する toJSON() がある場合には対応する
     *
     * @param obj
     * @return
     */
    @Override
    public JSONValue objectValue(Object obj) {
        // toJSON メソッドで変換
        String json = OMAP.toJSON(obj);
        if ( json != null ) {
            return JSON.parse(json);
        }
        return super.objectValue(obj);
    }
}
