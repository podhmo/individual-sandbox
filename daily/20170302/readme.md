# elastic search 自分で定義したanalyzerでは大文字小文字を区別しない

これだとだめ。上手く行かない。

```json
    "analysis" : {
      "analyzer" : {
        "my_ngram_analyzer" : {
          "tokenizer" : "my_ngram_tokenizer"
        }
      },
      "tokenizer" : {
        "my_ngram_tokenizer" : {
          "type" : "nGram",
          "min_gram" : "2",
          "max_gram" : "3",
          "token_chars": [ "letter", "digit", "symbol", "punctuation"]
        }
      }
    }
```

雑に対応するならfilterを追加すると良い。

```json
    "analysis" : {
      "analyzer" : {
        "my_ngram_analyzer" : {
          "tokenizer" : "my_ngram_tokenizer"
        },
        "my_normalize_ngram_analyzer": {
          "tokenizer" : "my_ngram_tokenizer",
          "filter": ["lowercase"],
          "char_filter": ["html_strip"]
        }
      },
      "tokenizer" : {
        "my_ngram_tokenizer" : {
          "type" : "nGram",
          "min_gram" : "2",
          "max_gram" : "3",
          "token_chars": [ "letter", "digit", "symbol", "punctuation"]
        }
      }
    }
  }
```

ただし、analyzerを指定しなかった場合にはcase insensitiveに検索できちゃうので注意。

```json
  "mappings" : {
    "value" : {
      "properties" : {
        "text" : { "type" : "text", "index": true },
        "text_mb": { "type": "text", "index": true, "analyzer": "my_ngram_analyzer" },
        "text_mb_icase": { "type": "text", "index": true, "analyzer": "my_normalize_ngram_analyzer" }
      }
    }
  }
```

詳細は [./example_elsasticsearch/](./example_elsasticsearch/)
