package net.siisise.json;

/**
 *
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
