package net.siisise.json.schema;

import java.util.List;
import java.util.Map;

/**
 *
 */
public class Definition {
    public String type; // object, integer, string
    public Map<String,Propertie> properties;
    public List<String> required;
    public String title;
}
