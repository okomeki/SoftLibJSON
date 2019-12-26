package net.siisise.json;

/**
 * JSONから特定の型に変換する.
 * 
 * @param <T>
 */
public interface JSONReplace<T> {
    
    /**
     * nullを返すと複数該当ありとする。
     * @return 
     */
    public Class<T> targetClass();
    
    public T replace(JSONValue json, Class<T> target);
    
}
