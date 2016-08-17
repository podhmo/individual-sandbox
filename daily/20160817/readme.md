# process置換の使い方

まだ慣れていないけれど。慣れると便利そうな感じはしている。

```bash
diff -u <(cat a.txt | sort) <(cat b.txt | sort)
```

# fmt example

- `%T` 値の方を表示
- `%v` 値を良い感じに表示
- `%#v` 値を型名やフィールド名も含めて冗長出力
- `%q` quoteされた文字列

あと添字アクセスは以下の様な感じ(1-origin)

```
fmt.Printf("type=%[1]T, value=%[1]v, verbose=%#[1]v\n", person)
```
