#[python][jqfpy]jqfpyのgetにちょっとだけ機能を追加した `get('[]/x/ys[]/z')`。

[https://github.com/podhmo/jqfpy:embed:cite]

jqfpyのgetにちょっとだけ機能を追加した。具体的には、入力がリストの場合の処理を追加した。

例えば、こういうデータのnameだけとか。skillsのnameだけを取りたい場合にリスト内包表記になってしまうのがちょっとだけだるかった。

```json
[
  {
    "name": "foo",
    "skills": [
      {
        "name": "x"
      },
      {
        "name": "y"
      },
      {
        "name": "z"
      }
    ]
  },
  {
    "name": "bar",
    "skills": [
      {
        "name": "x"
      },
      {
        "name": "y"
      }
    ]
  }
]
```

### 今まで

今まではリスト内包表記になってしまう。

```
$ jqfpy '[d["name"] for d in get()]' data.json --squash -r
foo
bar
```

skillsのnameの場合

```
$ jqfpy '[get("skills[]/name", d=d) for d in get()]' data.json --squash -c
["x", "y", "z"]
["x", "y"]
```

### これから

先頭が`[]`ならリストとして扱うことにした。ちょっと不格好だけれど。

```
$ jqfpy 'get("[]/name")' data.json --squash -r
foo
bar
```

skillsのnameの場合

```
$ jqfpy 'get("[]/skills[]/name")' data.json --squash -c
["x", "y", "z"]
["x", "y"]
```
