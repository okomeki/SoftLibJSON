package net.siisise.json.schema;

import java.util.List;

/**
 * まだ draft
 * https://json-schema.org/
 */
public class JSONSchema {
    static enum Type {
        object,
        array,
        string,
        number
    };
    
    Type type;
    
    List<String> required;

    // array
    int minItems;
    int maxItems;
    JSONSchema items;
    
    boolean additionalProperties;
    
    // string
    String pattern;
    int minLength;
    int maxLength;
    List<String> enum_; // Javaの予約語
    
    // number
    int minimum;
    int maximum;
    
    
    
}
