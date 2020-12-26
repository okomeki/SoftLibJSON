package net.siisise.omap;

import java.lang.reflect.Type;
import java.util.Collection;
import java.util.Map;

/**
 * 出力形式のプラグ。
 * あらかじめType T を想定した一括変換
 * @param <T>
 */
public interface MtoConvert<T> {
    /** 識別子
     * @return Tと同じ型
     */
    Type targetClass();
    <T> T nullValue();
    <T> T booleanValue(Boolean bool);
    <T> T numberValue(Number num);
    <T> T stringValue(CharSequence str);
    <T> T listValue(Collection list);
    <T> T arrayValue(Object array);
    <T> T mapValue(Map map);
    <T> T objectValue(Object obj);
}
