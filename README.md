# JSON

ABNFで RFC 8259 JSONとRFC 6901 JSON Pointer, RFC 6902 JSON Patch, RFC 7396 JSON Merge Patchを実装してみたもの

## リリース?

1.0.1 GitHubで使えるパッケージ名になおした。

## なにができる?

JSONのパース、オブジェクトマッピング、JSON Pointer, JSON Patch などがつかえるよ?

JavaのJSONPに準拠した実装もしてみたり。

実装はjsonとjson2 という名前で分かれているので主要な機能はjson2の方をつかってください。

storingifyはてきとーに実装しているのでまだ。

## 使用方法

SoftLibとSoftLibABNFとSoftLibJSONが必要です。

## 型情報

JavaとJSONの変換をするに当たって該当オブジェクトの中間型をとりあえず設けています。

pakage net.siisise.json2

- JSON2
- JSON2Value
    - JSON2NULL : null
    - JSON2Boolean : Boolean
    - JSON2Number : Number, BigInteger, BigDecimal
    - JSON2String : String, CharSequence
    - JSON2Collcetion
        - JSON2Array : List
        - JSON2Object : Map

JSON2の配列はJavaの配列、Listと変換でき、JSON2のObjectはJavaのObjectやMapと相互に変換できます。

Boolean,Number,StringもJavaのオブジェクトとして扱うこともでき、JSON2Valueな型として扱うこともできる状態で格納することができます。

JSON2Array<E> は List<E> として扱うことができ、JSONに変換可能なものを格納できます。

JSON2Object<E> は Map<String,E> として扱うことができ、JSONに変換可能なものを格納できます。

<E>は、Javaの型として扱いたい型を指定します。コンストラクタで型を指定することでJSONからの変換も適度に処理してくれます。

JSON Pointer, JSON PatchなどでArrayとObjectをまとめて扱うためにJSON2Collcetionを設けています。

Objectはfield(内部の変数)を変換します。beanなどにも対応は予定していたり。

ということで、ほとんど何も気にせずJavaとJSONの変換をこなしてくれます。

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

これ以降はjson1系の説明なのでちょっと古い

JavaScript からの例
オブジェクトを文字列に変換する

    JavaScript:JSON.stringify(obj)
    Java;JSON.stringify(obj)
    Java:JSONValue.valueOf(obj).toString();

文字列をオブジェクトに変換する

    JavaScript:JSON.parse(src)
    Java:JSON.parseToObj(src,class);
    Java:JSON.parse(src)

おぶじぇくとまっぴんぐ各種

## String to JSON2Value to String

    String str = "{ abc: def }";
    JSON2Value value = JSON2.parseWrap(str);
    String str = value.toString();

    String str = "文字列";
    JSON2String value = JSON2.valueOf(str);
    Boolean, Number系も同じ。
    str = value.map();

    List list = new ArrayList();
    (略)
    JSON2Array array = JSON2.valueOf(list);
    list = array.map();
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
