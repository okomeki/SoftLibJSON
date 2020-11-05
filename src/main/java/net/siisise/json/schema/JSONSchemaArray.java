package net.siisise.json.schema;

public class JSONSchemaArray extends JSONSchema {
    
    int minItems;
    int maxItems;
    JSONSchema items;
    
    boolean additionalProperties;
}
