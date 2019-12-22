package net.siisise.json.schema;

/**
 * JSONなどからJavaのクラスを自動生成すればいいのかな
 */
public class JSONClassConvert {
    
    String packageName;
    
    void setPackage(String name) {
        packageName = name;
    }
    
    public String toJavaClassSrc(String name, Definition def) {
        StringBuilder sb = new StringBuilder();
        
        sb.append("package ");
        sb.append(packageName);
        sb.append("\r\n");
        sb.append("\r\npublic class ");
        sb.append(name);
        sb.append(" {"); //
        
        for ( String key : def.properties.keySet() ) {
            Propertie val = def.properties.get(key);
            sb.append(ref(name, val));
        }

        sb.append("\r\n");
        
        sb.append("\r\n}\r\n");
        
        return sb.toString();
    }
    
    String ref(String name, Propertie val) {
        
        if ("object".equals(val.type)) {
            
        } else if ("array".equals(val.type)) {
            
        } else if ("boolean".equals(val.type)) {
            
        } else if ("number".equals(val.type)) {
            
        } else if ("string".equals(val.type)) {
            
        } else if ("null".equals(val.type)) {
            
        }
        throw new java.lang.UnsupportedOperationException();
    }
    
}
