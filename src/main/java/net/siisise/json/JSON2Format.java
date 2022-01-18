package net.siisise.json;

/**
 * 改行をなんとかするだけ。
 * JSON2Value#toString(JSON2Format) で使う
 */
public class JSON2Format {
    
    public final String crlf;
    public final String tab;

    /**
     * 改行やタブを入れるか省略するか決める要素
     * @param crlf 改行コードに相当する部分
     * @param tab タブに相当する部分
     */
    JSON2Format(String crlf, String tab) {
        this.crlf = crlf;
        this.tab = tab;
    }
}
