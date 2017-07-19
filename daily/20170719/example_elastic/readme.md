#[elasticsearch] elasticsearchでカタカナとひらがなを同列視して利用できるようにする

normalizeが必要。

## normalize

- Unicodeで正規化
- ひらがなをカタカナに変換

unicodeで正規化すると半角全角の対応まではできる。pluginが必要。

```bash
$ elasticsearch-plugin install --batch analysis-icu
```

以下のようなmappingでindex作成。

```json
{
  "settings": {
    "number_of_shards": 1,
    "number_of_replicas": 0,
    "refresh_interval": "1s",
    "index.requests.cache.enable": true,
    "analysis": {
      "filter": {
        "single_ascii_stop": {
          "type": "stop",
          "stopwords": [
            "a",
            "b",
            "c",
            "d",
            "e",
            "f",
            "g",
            "h",
            "i",
            "j",
            "k",
            "l",
            "m",
            "n",
            "o",
            "p",
            "q",
            "r",
            "s",
            "t",
            "u",
            "v",
            "w",
            "x",
            "y",
            "z",
            "0",
            "1",
            "2",
            "3",
            "4",
            "5",
            "6",
            "7",
            "8",
            "9"
          ]
        },
        "hiragana_to_kana": {
          "type": "icu_transform",
          "id": "Hiragana-Katakana"
        }
      },
      "analyzer": {
        "my_ngram_analyzer": {
          "tokenizer": "my_ngram_tokenizer",
          "char_filter": [
            "nfkc_normalizer"
          ],
          "filter": [
            "single_ascii_stop",
            "hiragana_to_kana"
          ]
        }
      },
      "tokenizer": {
        "my_ngram_tokenizer": {
          "type": "nGram",
          "min_gram": "1",
          "max_gram": "3"
        }
      },
      "char_filter": {
        "nfkc_normalizer": {
          "type": "icu_normalizer",
          "name": "nfkc",
          "mode": "compose"
        }
      }
    }
  },
  "mappings": {
    "site-search": {
      "dynamic": false,
      "_source": {
        "enabled": true
      },
      "_all": {
        "enabled": false
      },
      "properties": {
        "id": {
          "type": "keyword",
          "index": true
        },
        "name": {
          "type": "text",
          "index": true,
          "analyzer": "my_ngram_analyzer"
        },
        "page": {
          "properties": {
            "title": {
              "type": "text",
              "index": true,
              "analyzer": "my_ngram_analyzer"
            },
            "description": {
              "type": "text",
              "index": true,
              "analyzer": "my_ngram_analyzer"
            }
          }
        },
        "gaKeywords": {
          "type": "text",
          "index": true,
          "analyzer": "my_ngram_analyzer"
        },
        "createdAt": {
          "type": "date",
          "index": false
        }
      }
    }
  }
}

```

## データの登録

以下のことを調べたい

- 半角/全角を無視して検索できるか
- カタカナ/ひらがなを無視して検索できるか。
- (1:nのデータの検索がうまくいくかどうか)

1.jsonは正しい形
2.jsonはひらがな
3.jsonは半角

1.json
```json
{
  "name": "テストサイト",
  "page": {
    "title": "テストタイトル",
    "description": "テスト概要"
  },
  "gaKeywords": [
    "A",
    "dog",
    "test",
    "犬の王国"
  ]
}

```
2.json
```json
{
  "name": "てすとサイト",
  "page": {
    "title": "てすとタイトル",
    "description": "てすと概要"
  },
  "gaKeywords": [
    "A",
    "cat",
    "test",
    "猫の王国"
  ]
}

```
3.json
```json
{
  "name": "ﾃｽﾄサイト",
  "page": {
    "title": "ﾃｽﾄタイトル",
    "description": "ﾃｽﾄ概要"
  },
  "gaKeywords": [
    "A",
    "bird",
    "test",
    "鳥の王国"
  ]
}

```

## 検索

こんな感じ。

queries/1.json
```json
{
  "query": {
    "bool": {
      "must": {
        "query_string": {
          "query": "テスト"
        }
      }
    }
  }
}

```
outputs/1.json
```json
{
  "took" : 2,
  "timed_out" : false,
  "_shards" : {
    "total" : 1,
    "successful" : 1,
    "failed" : 0
  },
  "hits" : {
    "total" : 3,
    "max_score" : 0.830158,
    "hits" : [
      {
        "_index" : "site-entry-search",
        "_type" : "site-search",
        "_id" : "1",
        "_score" : 0.830158,
        "_source" : {
          "name" : "テストサイト",
          "page" : {
            "title" : "テストタイトル",
            "description" : "テスト概要"
          },
          "gaKeywords" : [
            "A",
            "dog",
            "test",
            "犬の王国"
          ]
        }
      },
      {
        "_index" : "site-entry-search",
        "_type" : "site-search",
        "_id" : "2",
        "_score" : 0.830158,
        "_source" : {
          "name" : "てすとサイト",
          "page" : {
            "title" : "てすとタイトル",
            "description" : "てすと概要"
          },
          "gaKeywords" : [
            "A",
            "cat",
            "test",
            "猫の王国"
          ]
        }
      },
      {
        "_index" : "site-entry-search",
        "_type" : "site-search",
        "_id" : "3",
        "_score" : 0.830158,
        "_source" : {
          "name" : "ﾃｽﾄサイト",
          "page" : {
            "title" : "ﾃｽﾄタイトル",
            "description" : "ﾃｽﾄ概要"
          },
          "gaKeywords" : [
            "A",
            "bird",
            "test",
            "鳥の王国"
          ]
        }
      }
    ]
  }
}

```
queries/2.json
```json
{
  "query": {
    "bool": {
      "must": {
        "query_string": {
          "query": "てすと"
        }
      }
    }
  }
}

```
outputs/2.json
```json
{
  "took" : 4,
  "timed_out" : false,
  "_shards" : {
    "total" : 1,
    "successful" : 1,
    "failed" : 0
  },
  "hits" : {
    "total" : 3,
    "max_score" : 0.830158,
    "hits" : [
      {
        "_index" : "site-entry-search",
        "_type" : "site-search",
        "_id" : "1",
        "_score" : 0.830158,
        "_source" : {
          "name" : "テストサイト",
          "page" : {
            "title" : "テストタイトル",
            "description" : "テスト概要"
          },
          "gaKeywords" : [
            "A",
            "dog",
            "test",
            "犬の王国"
          ]
        }
      },
      {
        "_index" : "site-entry-search",
        "_type" : "site-search",
        "_id" : "2",
        "_score" : 0.830158,
        "_source" : {
          "name" : "てすとサイト",
          "page" : {
            "title" : "てすとタイトル",
            "description" : "てすと概要"
          },
          "gaKeywords" : [
            "A",
            "cat",
            "test",
            "猫の王国"
          ]
        }
      },
      {
        "_index" : "site-entry-search",
        "_type" : "site-search",
        "_id" : "3",
        "_score" : 0.830158,
        "_source" : {
          "name" : "ﾃｽﾄサイト",
          "page" : {
            "title" : "ﾃｽﾄタイトル",
            "description" : "ﾃｽﾄ概要"
          },
          "gaKeywords" : [
            "A",
            "bird",
            "test",
            "鳥の王国"
          ]
        }
      }
    ]
  }
}

```
queries/3.json
```json
{
  "query": {
    "bool": {
      "must": {
       "query_string": {
         "query": "bird"
        }
      }
    }
  }
}

```
outputs/3.json
```json
{
  "took" : 3,
  "timed_out" : false,
  "_shards" : {
    "total" : 1,
    "successful" : 1,
    "failed" : 0
  },
  "hits" : {
    "total" : 1,
    "max_score" : 4.675513,
    "hits" : [
      {
        "_index" : "site-entry-search",
        "_type" : "site-search",
        "_id" : "3",
        "_score" : 4.675513,
        "_source" : {
          "name" : "ﾃｽﾄサイト",
          "page" : {
            "title" : "ﾃｽﾄタイトル",
            "description" : "ﾃｽﾄ概要"
          },
          "gaKeywords" : [
            "A",
            "bird",
            "test",
            "鳥の王国"
          ]
        }
      }
    ]
  }
}

```
queries/4.json
```json
{
  "query": {
    "bool": {
      "must": {
       "query_string": {
         "query": "猫"
        }
      }
    }
  }
}

```
outputs/4.json
```json
{
  "took" : 3,
  "timed_out" : false,
  "_shards" : {
    "total" : 1,
    "successful" : 1,
    "failed" : 0
  },
  "hits" : {
    "total" : 1,
    "max_score" : 0.9351026,
    "hits" : [
      {
        "_index" : "site-entry-search",
        "_type" : "site-search",
        "_id" : "2",
        "_score" : 0.9351026,
        "_source" : {
          "name" : "てすとサイト",
          "page" : {
            "title" : "てすとタイトル",
            "description" : "てすと概要"
          },
          "gaKeywords" : [
            "A",
            "cat",
            "test",
            "猫の王国"
          ]
        }
      }
    ]
  }
}

```
