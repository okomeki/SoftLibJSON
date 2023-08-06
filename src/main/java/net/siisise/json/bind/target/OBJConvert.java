package net.siisise.json.bind.target;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import net.siisise.bind.format.TypeFallFormat;
import net.siisise.bind.format.BindObject;

/**
 * JSON 系への変換ベース.
 * 配列をString系、ObjectをMap系に変換する.
 * 
 * @param <T> 
 */
public abstract class OBJConvert<T> extends TypeFallFormat<T> implements BindObject<T> {

    public static enum MapType {
        FIELD,
        DECLARED_FIELD,
        BEAN
    }

    /**
     * まだ固定.
     * 他未検証.
     */
    final MapType type = MapType.FIELD;
    
    /**
     * Java Objectの公開フィールドを取得してJSONObjectに変換する
     * toJSON() がある場合には対応する
     *
     * @param obj
     * @return
     */
    @Override
    public T objectFormat(Object obj) {
        return objectFormat(obj, type);
    }

    public T objectFormat(Object obj, MapType type) {
        Map<String, Object> objmap;

        switch (type) {
            case FIELD:
                objmap = fieldsToMap(obj);
                break;
            case DECLARED_FIELD:
                objmap = declaredFieldsToMap(obj);
                break;
            case BEAN:
                objmap = beanMap(obj);
                break;
            default:
                objmap = null;
        }
//        if ( objmap.isEmpty() ) { // 仮
//            return stringFormat(obj.toJSON());
//        }

        return mapFormat(objmap);
    }

    public static Map<String, Object> fieldsToMap(Object obj) {
        Class<? extends Object> cls = obj.getClass();
        HashMap objmap = new LinkedHashMap();

        Field[] fields = cls.getFields();

        for (Field field : fields) {
            try {
                objmap.put(field.getName(), field.get(obj));
            } catch (IllegalArgumentException | IllegalAccessException ex) {
                Logger.getLogger(OBJConvert.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
        return objmap;
    }

    public static Map<String, Object> declaredFieldsToMap(Object obj) {
        Class<? extends Object> cls = obj.getClass();
        HashMap objmap = new LinkedHashMap();

        while (cls != null) {
            Field[] fields = cls.getDeclaredFields();

            for (Field field : fields) {
                try {
//                    field.setAccessible(true);
                    objmap.put(field.getName(), field.get(obj));
                } catch (IllegalArgumentException | IllegalAccessException ex) {
                    Logger.getLogger(OBJConvert.class.getName()).log(Level.SEVERE, null, ex);
                }

            }
            cls = cls.getSuperclass();
        }

        return objmap;
    }

    public static Map<String, Object> beanMap(Object obj) {
        Class<? extends Object> cls = obj.getClass();
        HashMap objmap = new LinkedHashMap();

        while (cls != null) {
            Method[] methods = cls.getMethods();

            for (Method method : methods) {
                try {
                    if (method.getParameterCount() != 0 || method.getReturnType() != Void.TYPE) {
                        continue;
                    }
                    String name = method.getName();
                    String keyName;
                    if (name.startsWith("get")) {
                        keyName = name.substring(3, 4).toLowerCase();
                        if (name.length() > 4) {
                            keyName = keyName + name.substring(4);
                        }
                        objmap.put(keyName, method.invoke(obj));
                    } else if (name.startsWith("is")) {
                        keyName = name.substring(2, 3).toLowerCase();
                        if (name.length() > 3) {
                            keyName = keyName + name.substring(3);
                        }
                        objmap.put(keyName, method.invoke(obj));
                    }
                } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException ex) {
                    Logger.getLogger(OBJConvert.class.getName()).log(Level.SEVERE, null, ex);
                }

            }
            cls = cls.getSuperclass();
        }

        return objmap;
    }
}
