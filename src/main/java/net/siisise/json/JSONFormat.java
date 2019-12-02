package net.siisise.json;

/**
 * 改行をなんとかするだけ。
 */
public class JSONFormat {
    
    String crlf;
    String tab;

    JSONFormat(String crlf, String tab) {
        this.crlf = crlf;
        this.tab = tab;
    }
}
