package net.siisise.json;

/**
 *
 * @author okome
 */
public interface JSONReplacer {

    /**
     * JavaScriptのfunction だったもの
     * @param key
     * @param value
     * @return 
     */ 
    Object replacer(String key, Object value);
}
