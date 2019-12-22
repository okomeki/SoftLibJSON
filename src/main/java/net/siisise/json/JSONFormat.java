package net.siisise.json;

/**
 * 改行をなんとかするだけ。
 * JSON#toString(JSONFormat) で使う
 */
public class JSONFormat {
    
    String crlf;
    String tab;

    JSONFormat(String crlf, String tab) {
        this.crlf = crlf;
        this.tab = tab;
    }
}
