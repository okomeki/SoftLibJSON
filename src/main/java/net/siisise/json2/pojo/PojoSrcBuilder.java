package net.siisise.json2.pojo;

import java.util.Set;
import net.siisise.json2.JSON2Array;
import net.siisise.json2.JSON2Boolean;
import net.siisise.json2.JSON2NULL;
import net.siisise.json2.JSON2Number;
import net.siisise.json2.JSON2Object;
import net.siisise.json2.JSON2String;
import net.siisise.json2.JSON2Value;

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

    public String createSrc(JSON2Object json) {
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

    String typeTo(String name, JSON2Value obj) {
        if (obj instanceof JSON2Boolean) {
            return "boolean " + name;
        } else if (obj instanceof JSON2Array) {
            return "List<" + toListClassName(name) + "> " + name + "";
        } else if (obj instanceof JSON2Object) {
            return toClassName(name) + " " + name;
        } else if (obj instanceof JSON2String) {
            return "String " + name;
        } else if (obj instanceof JSON2Number) {
            return "Number " + name;
        } else if (obj instanceof JSON2NULL) {
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
