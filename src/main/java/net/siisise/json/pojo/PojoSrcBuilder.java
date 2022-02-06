package net.siisise.json.pojo;

import java.util.Set;
import net.siisise.json.JSONArray;
import net.siisise.json.JSONBoolean;
import net.siisise.json.JSONNULL;
import net.siisise.json.JSONNumber;
import net.siisise.json.JSONObject;
import net.siisise.json.JSONString;
import net.siisise.json.JSONValue;

/**
 * そういうのもあるといいのかもしれず 仮
 */
public class PojoSrcBuilder {

    String packageName;
    String className;

    public PojoSrcBuilder(String packageName, String className) {
        this.packageName = packageName;
        this.className = className;

    }

    public String createSrc(JSONObject json) {
        StringBuilder sb = new StringBuilder();

        sb.append("public class ");
        sb.append(className);
        sb.append(" {");

        for (String name : (Set<String>) json.keySet()) {
            sb.append("\r\n public ");
            sb.append(typeTo(name, json.getJSON(name)));
            sb.append(";");
        }
        sb.append("\r\n}");

        return sb.toString();
    }

    String typeTo(String name, JSONValue obj) {
        if (obj instanceof JSONBoolean) {
            return "boolean " + name;
        } else if (obj instanceof JSONArray) {
            return "List<" + toListClassName(name) + "> " + name + "";
        } else if (obj instanceof JSONObject) {
            return toClassName(name) + " " + name;
        } else if (obj instanceof JSONString) {
            return "String " + name;
        } else if (obj instanceof JSONNumber) {
            return "Number " + name;
        } else if (obj instanceof JSONNULL) {
            return "Object " + name;
        }
        throw new UnsupportedOperationException("未定義型");
    }

    String toClassName(String name) {
        return name.toUpperCase().charAt(0) + name.substring(1);

    }

    String toListClassName(String name) {
        name = toClassName(name);
        int len = (name.length() > 1 && name.charAt(name.length() - 1) == 's') ? name.length() - 1 : name.length();
        return name.substring(0, len);
    }

}
