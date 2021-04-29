package net.siisise.json;

public class JSONMember {

    public final String str;
    public final JSONValue value;

    public JSONMember(String str, JSONValue val) {
        this.str = str;
        this.value = val;
    }
}
