package net.siisise.json.bind.target;

import java.util.Collection;
import java.util.Map;
import net.siisise.bind.format.TypeBind;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONObject;
import net.siisise.json.JSONString;
import net.siisise.json.JSONValue;
import net.siisise.json.base.JSONBaseNULL;

/**
 * JSON抽象系からJSON各実装への変換.
 * List,Mapは表面をJSON系にラップするだけと、中もprimitive系に更新するのと2種類想定するかもしれない
 */
public class JSONConvert implements TypeBind<JSONValue> {

    @Override
    public JSONBaseNULL nullFormat() {
        return JSONBaseNULL.NULL;
    }

    @Override
    public JSONBoolean booleanFormat(boolean bool) {
        return bool ? JSONBoolean.TRUE : JSONBoolean.FALSE;
    }

    @Override
    public JSONNumber numberFormat(Number num) {
        return new JSONNumber(num);
    }

    @Override
    public JSONValue stringFormat(String str) {
        return new JSONString(str);
    }

    /**
     * 表面のみ変換
     * @param list
     * @return 
     */
    @Override
    public JSONArray collectionFormat(Collection list) {
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
    public JSONObject mapFormat(Map map) {
        if ( map instanceof JSONObject ) {
            return (JSONObject) map;
        }
        JSONObject obj = new JSONObject();
        ((Map<?,?>)map).forEach((k,v) -> {
            obj.put(k.toString(), v);
        });
        return obj;
    }

}
