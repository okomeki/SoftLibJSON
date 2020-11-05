package net.siisise.json2;

import net.siisise.json.JSONReplacer;
import net.siisise.json2.map.JSON2ArrayM;
import net.siisise.json2.map.JSON2NumberM;
import net.siisise.json2.map.JSON2ObjectM;
import net.siisise.json2.map.JSON2StringM;
import net.siisise.json2.map.JSON2ValueM;

/**
 *
 */
public class JSON2Map {

    JSON2ReplaceOM[] parsers = {
        new JSON2ValueM(),
        new JSON2NumberM(),
        new JSON2StringM(),
        new JSON2ArrayM(),
        new JSON2ObjectM()
    };

    Object valueOf(Object src) {
        return valueOf(src, null);
    }

    Object valueOf(Object src, JSONReplacer replacer) {
        for ( JSON2ReplaceOM ps : parsers ) {
            Object val = ps.value2Of(src, replacer);
            if ( val != ps ) {
                return val;
            }
        }
        return JSON2Object.convObject(src);
    }

    
}
