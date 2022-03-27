# SoftLibJSON for Java

ABNFで RFC 8259 JSON とRFC 6901 JSON Pointer, RFC 6902 JSON Patch, RFC 7396 JSON Merge Patchを実装してみたもの
Java API for JSON Processing (JSR-374)にも対応
Object Mappingは適度に実装 (Java API for JSON Bindingもかぶせたが互換性は不明)

## なにができる?

- ABNFを利用したJSONのパース
- ストリーム系のJSONパース
- オブジェクトマッピング JavaオブジェクトとJSON,JSON P(JSR-374)の適度な相互変換
- JSON Pointer
- JSON Patch と diff
- JSON Merge Patch と diff
- JSON Binding

などがつかえるよ?

JavaのJSONP,JSONBに準拠した実装もしてみたり。

storingifyはてきとーに実装しているのでまだ。

## 使用方法

SoftLibとSoftLibABNFとSoftLibJSONが必要です。

## Maven
~~~
<dependency>
  <groupId>net.siisise</groupId>
  <artifactId>softlib-json</artifaceId>
  <version>1.2.0</version>
  <type>jar</type>
</dependency>
~~~
## 型情報

JavaとJSONの変換をするに当たって該当オブジェクトの中間型をとりあえず設けています。

pakage net.siisise.json

- JSON
- JSONValue : Object
    - JSONNULL : null
    - JSONBoolean : Boolean
    - JSONNumber : Number, BigInteger, BigDecimal
    - JSONString : String, CharSequence
    - JSONCollcetion
        - JSONArray : List, 配列, コンストラクタ
        - JSONObject : Map, Object
    - JsonStructure
        - JSONPArray : JSONP用Array
        - JSONPObject : JSONP用Object

JSONValue がJSONとしての元になる型です。
JSONは、変換機能などを設けています。


JSONの配列JSONArrayはJavaの配列、Listとして扱うことができ、JSONObjectはJavaのMapとして扱えます。内部はJavaの値でBoolean,Number,String,List,Mapなどの形式で保存されています。

JSON.valueOf()でJSONValueに変換できそうなものはそのまま格納して問題ありません。

JSONCollection は、getJSON(), setJSON(), addJSON() などのメソッドでJSONValue形式の値を扱うことができます。

JSONArray&lt;E&gt; は List&lt;E&gt; として扱うことができ、JSONに変換可能なものを格納できます。

JSONObject&lt;E&gt; は Map&lt;String,E&gt; として扱うことができ、JSONに変換可能なものを格納できます。

&lt;E&gt;は、Javaの型として扱いたい型を指定します。コンストラクタで型を指定することでJSONからの変換も適度に処理してくれます。

streamでは、JSONArrayがj2stream()でJSONValueな値のparallelStream().map(JSON2::valueOf)相当を作れます。

JSONObjectのj2forEachで(String,JSONValue)なkey,valueのforEach(k,v)が使えます。JSONArrayはj2stream().forEach(e)で作れます。

JSONNumberはJSONからの変換ではmap()で取り出せる内部型にBigInteger型またはBigDecimal型を持っています。また、JSONNumberもNumberを継承しています。いずれもNumber型として扱えるのでIntegerなど適切な型に変換してから使います。

JSON Pointer, JSON PatchなどでArrayとObjectをまとめて扱うためにJSONCollcetionを設けています。

Objectはfield(内部の変数)を変換します。beanなどにも対応は予定していたり。

ということで、ほとんど何も気にせずJavaとJSONの変換をこなしてくれます。

JSON B 相当の変換関連はnet.siissie.omap.OMAP などにまとめています。
OMAP.valueOf(ソース,型) がJSON2Value.typeMap(型) の実体です。

公開や更新したくない要素などセキュリティには注意してください。

## 例?

val はJSONValue、obj は Javaの型として

JSONをparseしてJava Objcetに変換する

    byte[] json; UTF-8限定 または String json;

    JSONValue val = JSON.parseWrap(json); JSONValue型 (JSONArray,JSONObject, JSONStrign, JSONNumber, JSONBooleanなど)として受けたい場合
    Objcet obj = val.map();
    Object obj = JSON.parse(json); Object型 (ListやMap,String,Integer,Booleanなど)として直接結果を受けたい場合

Java由来のものをJSONに変換する

    JSONValue val = JSON.valueOf( Object object )
    out.println(val); または val.toString();

json2から特定の型のJava classに変換する

    ObjectX obj = val.typeMap(ObjcetX.class);
    int i = val.typeMap(Integer.TYPE);

json から JavaのJSONPっぽいものに変換する

    JsonValue jsonp = val.toJson();

JSONArray, JSONObject は stream も対応しているかもしれません。

JavaScript からの例
オブジェクトを文字列に変換する

    JavaScript:JSON.stringify(obj)
    Java;JSON.stringify(obj); //  まだかも
    Java:JSON.valueOf(obj).toString();

JSON文字列をオブジェクトに変換する

    JavaScript:JSON.parse(src)
    Java:JSON.parseWrap(src,class); JSONValueへ
    Java:JSON.parse(src); Javaへ  
    
おぶじぇくとまっぴんぐ各種

## String to JSONValue to String

    String json = "{ abc: def }";
    JSONValue value = JSON.parseWrap(json);
    String json = value.toString();
    
    JSONValue#toString() はJSON書式で出力します JSONStringなどの値はmap()またはtypeMap(Type) で取得します。
    TypeはClassの互換で<型情報>などをまとめたものです。

    String str = "文字列";
    JSONString value = JSON.valueOf(str);
    Boolean, Number系も同じ。
    str = value.map();

    List list = new ArrayList();
    (略)
    JSONArray array = JSON.valueOf(list);
    list = array.map(); // Listと互換なのでarrayをそのまま返します
    list = array.typeMap(List.class);

    String[] stringArray = {略};
    JSONArray array = JSON.valueOf(stringArray);
    stringArray = array.map(はいれつのくらす);

    int[] abc = {1,2,3};
    JSONArray array = JSON.valueOf(abc);
    abc = array.typeMap(int[]のくらす);
    abc = array.toArray(new int[0]); // も可 new

object は publicなデータを持つおぶじぇくと

    class A {
     int[] b;
     String c;
     List d;
    }

    A object;
    JSONObject obj = JSON.valueOf(object);
    JSONArray numVal = (JSONArray)obj.get("b");
    object = obj.typeMap(A.class);

    Map map = new HashMap();
    JSONObject obj = JSON.valueOf(map);
    map = obj.typeMap(Map.class);
