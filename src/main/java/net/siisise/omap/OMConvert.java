package net.siisise.omap;

/**
 *
 */
public interface OMConvert {
    
    /**
     * 参考程度
     * @return 
     */
    Class[] getSrcClasses();

    /**
     * 出力先が汎用なもので確定している場合
     * @param <T>
     * @param obj 変換もと
     * @param outConvert 変換器
     * @param replacer 変換器2 とりあえず外してある
     * @return 
     */
    <T> T valueOf(Object obj, MtoConvert<T> outConvert);

}
