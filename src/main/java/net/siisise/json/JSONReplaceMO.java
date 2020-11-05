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
    
    /**
     * 
     * @param json
     * @param target 型情報 #targetClass() と同じかもしれない。Typeにするかもしれない。とりあえず渡す
     * @return 
     */
    T replace(JSONValue json, Class<T> target);
    
}
