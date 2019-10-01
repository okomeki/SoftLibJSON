package net.siisise.json.jwt;

import java.io.UnsupportedEncodingException;
import java.util.logging.Level;
import java.util.logging.Logger;
import net.siisise.io.BASE64;
import net.siisise.io.Packet;

/**
 * JSON Web Signature (JWS)
 * BASE64URL(UTF8(JWS Protected Header)) || . ||
 * BASE64URL(JWS Payload) || . ||
 * BASE64URL(JWS Signature)
 * 
 * @see https://tools.ietf.org/html/rfc7515
 */
public class JWS7515 {
    
    String jws() {
        StringBuilder sb = new StringBuilder();
        BASE64 b64 = new BASE64(BASE64.URL,0);
        
        try {
            sb.append(b64.encode(jwsProtectedHeader().getBytes("utf-8")));
            sb.append(".");
            sb.append(b64.encode(jwsPayload().getBytes("utf-8")));
            sb.append(".");
            sb.append(b64.encode(jwsSignature().getBytes("utf-8")));
            return sb.toString();
        } catch (UnsupportedEncodingException ex) {
            Logger.getLogger(JWS7515.class.getName()).log(Level.SEVERE, null, ex); // ない
        }
        throw new java.lang.IllegalStateException();
    }

    String jwsProtectedHeader() {
        throw new java.lang.UnsupportedOperationException("jwsProtectedHeader");
    }

    String jwsPayload() {
        throw new java.lang.UnsupportedOperationException("jwsPayload");
    }
    
    String jwsSignature() {
        throw new java.lang.UnsupportedOperationException("jwsSignature");
    }
    
    /**
     * かるいてすと
     * @param argv 
     */
    public static void main(String[] argv) {
        try {
            String val = new String(BASE64.decodeURL("eyJpc3MiOiJqb2UiLA0KICJleHAiOjEzMDA4MTkzODAsDQogImh0dHA6Ly9leGFt\r\n" +
"cGxlLmNvbS9pc19yb290Ijp0cnVlfQ"),"utf-8");
            System.out.println(val);
            BASE64 burl = new BASE64(BASE64.URL,0);
            System.out.println(burl.encode(val.getBytes("utf-8")));
            byte[] d;
            d = BASE64.decodeURL("eujp");
            System.out.println("dlen " + d.length);
            d = BASE64.decodeURL("eeeeeuk");
            System.out.println("dlen " + d.length);
        } catch (UnsupportedEncodingException ex) {
            Logger.getLogger(JWS7515.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
}
