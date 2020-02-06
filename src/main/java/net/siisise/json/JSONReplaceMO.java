package net.siisise.json;

/**
 * JSONから特定の型に変換する.
 * 
 * @param <T>
 */
public interface JSONReplaceMO<T> {
    
    /**
     * nullを返すと複数該当ありとする。
     * @return 
     */
    Class<T> targetClass();
    
    T replace(JSONValue json, Class<T> target);
    
}
