package net.siisise.json.pointer;

import java.nio.charset.MalformedInputException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import net.siisise.abnf.AbstractABNF;
import net.siisise.io.Packet;

/**
 * RFC 6901 JSON Pointer
 *
 * @author okome twitter.okomeki
 */
public class JSONPointer {

    String[] path;

    public JSONPointer(String escapedPath) {
        if (!JSONPointerReg.jsonPointer.eq(escapedPath)) {
            throw new java.lang.UnsupportedOperationException();
        }
        this.path = escapedPath.split("/");
    }

    JSONPointer(List<Packet> lp) {
        path = new String[lp.size() + 1];
        path[0] = "";
        int i = 1;
        for (Packet p : lp) {
            path[i] = AbstractABNF.str(p);
            if (!JSONPointerReg.referenceToken.eq(path[i++])) {
                throw new java.lang.UnsupportedOperationException();
            }
        }
    }

    public JSONPointer sub() {
        List<Packet> lp = new ArrayList<>();
        if (path.length <= 1) {
            return null;
        }
        for (int i = 2; i < path.length; i++) {
            lp.add(AbstractABNF.pac(path[i]));
        }
        return new JSONPointer(lp);
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder(100);
        for (int i = 0; i < path.length; i++) {
            if (i != 0) {
                sb.append("/");
            }
            sb.append(path[i]);
        }
        return sb.toString();
    }

    public String[] toDecodeString() {
        String[] dec = new String[path.length];
        for (int i = 0; i < path.length; i++) {
            try {
                dec[i] = decode(path[i]);
            } catch (MalformedInputException ex) {
                throw new java.lang.UnsupportedOperationException();
//                Logger.getLogger(JSONPointer.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return dec;
    }

    /**
     * RFC 3986
     *
     * @return
     */
    String toURIEncode() {
        StringBuilder sb = new StringBuilder();
        List<String> n = Arrays.asList(path);
        n.remove(0);
        for (String s : n) {
            sb.append("/");
            try {
                sb.append(urlEnc(decode(s)));
            } catch (MalformedInputException ex) {
                throw new java.lang.UnsupportedOperationException();
            }
        }
        return sb.toString();
    }

    static String decode(String str) throws MalformedInputException {
        StringBuilder sb = new StringBuilder(100);
        StringBuilder src = new StringBuilder(str);
        char c;
        while (src.length() > 0) {
            c = src.charAt(0);
            if (c == '~') {
                switch (src.charAt(1)) {
                    case '0':
                        c = '~';
                        break;
                    case '1':
                        c = '/';
                        break;
                    default:
                        throw new java.nio.charset.MalformedInputException(0);
                }
                src.delete(0, 2);
            } else {
                src.deleteCharAt(0);
            }
            sb.append(c);
        }
        return sb.toString();
    }

    /**
     * utf-16? RFC 3986
     *
     * @param str
     * @return
     */
    static String urlEnc(String str) {
        char[] chs = str.toCharArray();
        StringBuilder sb = new StringBuilder(str.length() * 2);
        for (char ch : chs) {
            if (ch < 0x20) {
                sb.append("%");
                sb.append(Integer.toHexString(0x100 + ch).substring(1));
            } else {
                sb.append(ch);

            }
        }
        return sb.toString();
    }

}
