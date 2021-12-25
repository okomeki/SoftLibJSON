package net.siisise.json.mergepatch;

import net.siisise.json2.JSON2NULL;
import net.siisise.json2.JSON2Object;
import net.siisise.json2.JSON2Value;

/**
 * RFC 7396 JSON Merge Patch.
 * 
 * diff相当もほしいかも
 * 
 * RFC 7159 JSON
 * RFC 5789 PATCH Method for HTTP
 * @see https://tools.ietf.org/html/rfc7396
 */
public class JSONMergePatch7396 {
    
    // 4.IANA Considerations
    public static final String typeName = "application"; 
    public static final String subtypeName = "merge-patch+json";
    
    /**
     * おりじなるとぱっちから更新結果を返す
     * @param target
     * @param patch
     * @return result
     */
    public static JSON2Value mergePatch(JSON2Value target, JSON2Value patch) {
        if ( patch instanceof JSON2Object ) {
            if ( !(target instanceof JSON2Object) ) {
                target = new JSON2Object();
            }
            for ( String name : ((JSON2Object<?>)patch).keySet() ) {
                JSON2Value v = ((JSON2Object) patch).getJSON(name);
                if ( v instanceof JSON2NULL ) {
                    if ( ((JSON2Object<?>)target).get(name) != null ) {
                        ((JSON2Object<?>)target).remove(name);
                    }
                } else {
                    ((JSON2Object<?>)target).addJSON(name, mergePatch(((JSON2Object)target).getJSON(name), v));
                }
            }
            return target;
        } else {
            return patch;
        }
    }
    
    /**
     * JSONMergePatchをつくろう
     * 差分の自動生成がなかったので作ってみるか
     * @param original
     * @param result
     * @return patch
     */
    public static JSON2Value diff(JSON2Value original, JSON2Value result) {
        if ( (result instanceof JSON2Object)) {
            
        } else {
            return result;
        }
        throw new java.lang.UnsupportedOperationException("yet.");
    }
}
