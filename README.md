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
  <version>1.1.3</version>
  <type>jar</type>
</dependency>
~~~
## 型情報

JavaとJSONの変換をするに当たって該当オブジェクトの中間型をとりあえず設けています。

pakage net.siisise.json

- JSON2
- JSON2Value : Object
    - JSON2NULL : null
    - JSON2Boolean : Boolean
    - JSON2Number : Number, BigInteger, BigDecimal
    - JSON2String : String, CharSequence
    - JSON2Collcetion
        - JSON2Array : List, 配列, コンストラクタ
        - JSON2Object : Map, Object
    - JsonStructure
        - JSONPArray : JSONP用Array
        - JSONPObject : JSONP用Object

JSON2Value がJSONとしての元になる型です。
JSON2は、変換機能などを設けています。


JSON2の配列JSON2ArrayはJavaの配列、Listとして扱うことができ、JSON2ObjectはJavaのMapとして扱えます。内部はJavaの値でBoolean,Number,String,List,Mapなどの形式で保存されています。

JSON2.valueOf()でJSON2Valueに変換できそうなものはそのまま格納して問題ありません。

JSON2Collection は、getJSON(), setJSON(), addJSON() などのメソッドでJSON2Value形式の値を扱うことができます。

JSON2Array<E> は List&lt;E&gt; として扱うことができ、JSONに変換可能なものを格納できます。

JSON2Object<E> は Map&lt;String,E&gt; として扱うことができ、JSONに変換可能なものを格納できます。

&lt;E&gt;は、Javaの型として扱いたい型を指定します。コンストラクタで型を指定することでJSONからの変換も適度に処理してくれます。

streamでは、JSON2Arrayがj2stream()でJSON2Valueな値のparallelStream().map(JSON2::valueOf)相当を作れます。

JSON2Objectのj2forEachで(String,JSON2Value)なkey,valueのforEach(k,v)が使えます。JSON2Arrayはj2stream().forEach(e)で作れます。

JSON2NumberはJSONからの変換ではmap()で取り出せる内部型にBigInteger型またはBigDecimal型を持っています。また、JSON2NumberもNumberを継承しています。いずれもNumber型として扱えるのでIntegerなど適切な型に変換してから使います。

JSON Pointer, JSON PatchなどでArrayとObjectをまとめて扱うためにJSON2Collcetionを設けています。

Objectはfield(内部の変数)を変換します。beanなどにも対応は予定していたり。

ということで、ほとんど何も気にせずJavaとJSONの変換をこなしてくれます。

JSON B 相当の変換関連はnet.siissie.omap.OMAP などにまとめています。
OMAP.valueOf(ソース,型) がJSON2Value.typeMap(型) の実体です。

公開や更新したくない要素などセキュリティには注意してください。

## 例?

val はJSON2Value、obj は Javaの型として

JSONをparseしてJava Objcetに変換する

    byte[] json; UTF-8限定 または String json;

    JSON2Value val = JSON2.parseWrap(json); JSON2Value型 (JSON2Array,JSON2Object, JSON2Strign, JSON2Number, JSON2Booleanなど)として受けたい場合
    Objcet obj = val.map();
    Object obj = JSON2.parse(json); Object型 (ListやMap,String,Integer,Booleanなど)として直接結果を受けたい場合

Java由来のものをJSONに変換する

    JSON2Value val = JSON2.valueOf( Object object )
    out.println(val); または val.toString();

json2から特定の型のJava classに変換する

    ObjectX obj = val.typeMap(ObjcetX.class);
    int i = val.typeMap(Integer.TYPE);

json2 から JavaのJSONPっぽいものに変換する

    JsonValue jsonp = val.toJson();

JSON2Array, JSON2Object は stream も対応しているかもしれません。

JavaScript からの例
オブジェクトを文字列に変換する

    JavaScript:JSON.stringify(obj)
    Java;JSON2.stringify(obj); //  まだかも
    Java:JSON2.valueOf(obj).toString();

JSON文字列をオブジェクトに変換する

    JavaScript:JSON.parse(src)
    Java:JSON2.parseWrap(src,class); JSON2Valueへ
    Java:JSON2.parse(src); Javaへ  
    
おぶじぇくとまっぴんぐ各種

## String to JSON2Value to String

    String json = "{ abc: def }";
    JSON2Value value = JSON2.parseWrap(json);
    String json = value.toString();
    
    JSON2Value#toString() はJSON書式で出力します JSON2Stringなどの値はmap()またはtypeMap(Type) で取得します。
    TypeはClassの互換で<型情報>などをまとめたものです。

    String str = "文字列";
    JSON2String value = JSON2.valueOf(str);
    Boolean, Number系も同じ。
    str = value.map();

    List list = new ArrayList();
    (略)
    JSON2Array array = JSON2.valueOf(list);
    list = array.map(); // Listと互換なのでarrayをそのまま返します
    list = array.typeMap(List.class);

    String[] stringArray = {略};
    JSON2Array array = JSON2.valueOf(stringArray);
    stringArray = array.map(はいれつのくらす);

    int[] abc = {1,2,3};
    JSON2Array array = JSON2.valueOf(abc);
    abc = array.typeMap(int[]のくらす);
    abc = array.toArray(new int[0]); // も可 new

object は publicなデータを持つおぶじぇくと

    class A {
     int[] b;
     String c;
     List d;
    }

    A object;
    JSON2Object obj = JSON2.valueOf(object);
    JSON2Array numVal = (JSON2Array)obj.get("b");
    object = obj.typeMap(A.class);

    Map map = new HashMap();
    JSON2Object obj = JSON2.valueOf(map);
    map = obj.typeMap(Map.class);
