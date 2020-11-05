package net.siisise.json2;

import net.siisise.json.JSONReplacer;

public interface JSON2ReplaceOM {
    /**
     * 該当しない場合はnullではなくReplace自身を返す
     * @param obj
     * @param replacer
     * @return 
     */
    Object value2Of(Object obj, JSONReplacer replacer);
}
