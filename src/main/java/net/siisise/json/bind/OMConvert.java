package net.siisise.json.bind;

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
     * @return 未確定な場合はoutConvert を返す
     */
    <T> T valueOf(Object obj, MtoConvert<T> outConvert);

}
