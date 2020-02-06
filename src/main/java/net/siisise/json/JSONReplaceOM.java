package net.siisise.json;

/**
 * JSONに変換する
 */
public interface JSONReplaceOM {
    JSONValue valueOf(Object obj, JSONReplacer replacer);
}
