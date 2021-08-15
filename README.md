# JSON

ABNFで RFC 8259 JSONとRFC 6901 JSON Pointer, RFC 6902 JSON Patch, RFC 7396 JSON Merge Patchを実装してみたもの

## ダウンロード

 https://siisise.net/softlib/

SoftLibとABNFとJSONが必要です。

## 例?

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

## String to JSONValue to String

    String str = "{ abc: def }";
    JSONValue value = JSON8259Reg.parse(str);
    String str = value.toString();

    String str = "文字列";
    JSONString value = JSONValue.valueOf(str);
    Boolean, Number系も同じ。
    str = value.value();

    List list = new ArrayList();
    (略)
    JSONArray array = JSONValue.valueOf(list);
    list = array.value();
    list = array.map(List.class);

    String[] stringArray = {略};
    JSONArray array = JSONValue.valueOf(stringArray);
    stringArray = array.map(はいれつのくらす);

    int[] abc = {1,2,3};
    JSONArray array = JSONValue.valueOf(abc);
    abc = array.map(int[]のくらす);
    abc = array.toArray(new int[0]); // も可 new

object は publicなデータを持つおぶじぇくと

    class A {
     int[] b;
     String c;
     List d;
    }

    A object;
    JSONObject obj = JSONValue.valueOf(object);
    JSONArray numVal = (JSONArray)obj.get("b");
    object = obj.map(A.class);

    Map map = new HashMap();
    JSONObject obj = JSONValue.valueOf(map);
    map = obj.map(Map.class);
