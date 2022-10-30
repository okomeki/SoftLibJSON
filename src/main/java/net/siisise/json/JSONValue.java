/*
 * Copyright 2022 Siisise Net
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.siisise.json;

import java.lang.reflect.Type;
import javax.json.JsonValue;

/**
 * 要素的なJSON。
 
 JSON2Object implements Map 可変
 JSON2Array implements List 可変
 JSON2Number implements Number 不変
 JSON2String CharSequence になれない 互換なし 不変
 JSON2Boolean 互換なし 不変
 JSON2NULL 互換なし 不変

 toJSON() をJSON出力にすること
 JSON2ObjectとJSON2ArrayはMapとListとして内容の変更が可能
 JSON2を継承する必要はなくなったのでどこかで外す。
 */
public interface JSONValue {
    
    /**
     * JSON (JavaのString)として出力する.
     * 主な用途はデータ交換なので改行などは省略したい。
     * 改行などつけたい場合はJSON2Formatをつける
     * JsonValue系がtoJson() を利用するのと分けておく
     * @return JSON文字列
     */
    String toJSON();
    
    /**
     * JSONではなくていいのでプリミティブ型に近い文字列として
     * 特に指定の書式がなければJSONとして出力する
     * @return 文字表現
     */
    @Override
    String toString();
    

    public static final JSONFormat NOBR = new JSONFormat("","");
    public static final JSONFormat TAB = new JSONFormat("\r\n","  ");

    /**
     * 書式を指定してJSONとして出力する.
     *
     * @param format
     * @return
     */
    String toJSON(JSONFormat format);

    /**
     * 固定のJava寄りの型に変換する。
     * List,Map,primitive対応データ型など
     * array, object, またはその中身は型変換が介入しない場合は複製されない
     * @param <T>
     * @return 
     */
    <T> T map();
    
    /**
     * 可能な限り指定型に変換する。
     * Fieldから
     * List,Map,配列,Java Object, Genericなどまで可
     * JSON-B では fromJson 相当
     * 
     * @param <T>
     * @param type Classの他、Fieldから取得できる型情報ParameterizedTypeに対応する
     * @return 
     */
    <T> T typeMap(Type type);
    
    /**
     * Jakarta JSON-P系に準拠する.
     * @return JSON-P 的なもの
     */
    JsonValue toJson();

    /**
     * 旧 javax系
     * @return JSON-P的なもの 
     */
    javax.json.JsonValue toXJson();

}
