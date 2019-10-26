package net.siisise.json;

/**
 *
 */
public class JSONMember {

    public final JSONString str;
    public final JSONValue value;

    public JSONMember(JSONString str, JSONValue val) {
        this.str = str;
        this.value = val;
    }
}
