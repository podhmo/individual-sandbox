元の記事はこれ

- [jq コマンドを使う日常のご紹介 - Qiita](http://qiita.com/takeshinoda@github/items/2dec7a72930ec1f658af)

## 使いかたの部分

ただの整形

```
$ cat data.json | jqfpy
{
  "items": [
    {
      "item_id": 1,
      "name": "すてきな雑貨",
      "price": 2500
    },
    {
      "item_id": 2,
      "name": "格好いい置物",
      "price": 4500
    }
  ]
}
```

値の列挙

```
$ cat data.json | jqfpy '[x["name"] for x in get("items")]'
[
  "すてきな雑貨",
  "格好いい置物"
]
```

ダブルクォートが邪魔

```
$ cat data.json | jqfpy -r --squash '[x["name"] for x in get("items")]'
すてきな雑貨
格好いい置物
```

フィルタ（パイプ）(略)

集計

```
$ cat data.json | jqfpy 'sum(int(x["price"]) for x in get("items"))'
7000
```

フィルタを通じて再整形

```
$ cat data.json | jqfpy --squash '[{"name": x["name"], "yen": x["price"]} for x in get("items")]'
{
  "name": "すてきな雑貨",
  "yen": 2500
}
{
  "name": "格好いい置物",
  "yen": 4500
}
```

map(略),reduce(略)


## おまけ

カンマは区切り文字じゃ無くて、フィルタの入力を分けるの部分

```
$ echo '{"key1": "val1", "key2": "val2"}' | jqfpy '[get("key1"), get("key2")]'
[
  "val1",
  "val2"
]
```

```
$ echo '{"key1": "val1", "key2": "val2"}' | jqfpy '[get("key1"), list(get().keys())]'
[
  "val1",
  [
    "key1",
    "key2"
  ]
]
```

Key/Valueをオブジェクトに変換するの部分


```
$ echo '[{"Key": "tagkey", "Value": "value"},{"Key": "tagkey2", "Value": "value２"}]' | jqfpy 'dict([xs.values() for xs in get()])'
{
  "tagkey": "value",
  "tagkey2": "value２"
}
```

## それっぽい活用事例

ある人のgithubリポジトリを列挙する

```
$ reposurl=`curl -s curl -s https://api.github.com/users/takeshinoda | jqfpy -r 'get("repos_url")'`
$ curl -s $reposurl | jqfpy '[x["name"] for x in get()]'
[
  "ansible-mackerel-agent",
  "avr-test",
  "babel-lambda-template",
  "ctmcp",
  "doctree",
  "Dynamoid",
  "HogeApp",
  "mizukiri",
  "mizukiri-demo",
  "mysql2",
  "node-aws-lambda",
  "node-jq-ext",
  "pfds",
  "php2haml_preprocessor",
  "rack-vcr",
  "rails-dockerfile",
  "rawscsi",
  "rubykaigi",
  "serverside-jq-serverless",
  "sjs-sample",
  "sprk2012-cflt",
  "test-queue",
  "thinreports-handler",
  "thinreports-rails",
  "uuidgen"
]
```

東京の気温の履歴(apikeyがひつようだった)

```
curl -s 'http://api.openweathermap.org/data/2.5/history/city?id=1850147&type=hour' | jqfpy -r '[[x["dt"], get("main/temp_min", x), get("main/temp_max")]'
```

## SQLっぽく使う

3つ以上売れた商品の名前を出す

```
$ cat data2.json | jqfpy 'from collections import defaultdict; items = get("items"); orders = get("item_orders"); c = defaultdict(list); [c[x["item_id"]].append(x) for x in orders]; [items[item_id] for item_id, values in c.items() if 3 <= sum(int(x["number"]) for x in values)]'
```
