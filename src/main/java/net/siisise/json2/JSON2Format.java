package net.siisise.json2;

/**
 * 改行をなんとかするだけ。
 * JSON2Value#toString(JSONFormat) で使う
 */
public class JSON2Format {
    
    public String crlf;
    public String tab;

    JSON2Format(String crlf, String tab) {
        this.crlf = crlf;
        this.tab = tab;
    }
}
