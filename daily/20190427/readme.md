## python discord.py

```console
$ git clone git@github.com:Rapptz/discord.py
```

### music bot

小さめの実装を作ってみるか。と思ったら特に作る必要がなかった。

https://github.com/Rapptz/discord.py/blob/master/examples/basic_voice.py

### archで動かす

- https://wiki.archlinux.jp/index.php/FFmpeg


```console
$ yay -S ffmpeg
$ pip install youtube_dl
$ pip install -e .[voice]
```

### botのアカウントを作成

https://discordapp.com/developers/applications/



## jsonknife separation

- bundled file -> sepratated files への変換
- 複数のファイルへの出力何か方法があったっけ？

  - migrationのときのコードが使えるかも？
  - ファイルの階層構造どうしようかな？

### :warning: 自己への再帰に注意

```yaml
definitions:
  Schema:
    type: object
    patternProperties:
      "^[a-zA-Z0-9_]":
        oneOf:
          - { $ref: "#/definitions/Schema" }
          - { $ref: "#/definitions/Reference" }
  Reference:
    type: object
    properties
      $ref:
        type: string
    required:
      - $ref
```

### :warning: 出力後の表現

ファイルの階層と全体の階層構造が一致していたほうが便利。

```
.
├── definitions
│   ├── Reference.yaml
│   └── Schema.yaml
└── main.yaml

1 directory, 3 files
```

このときのdefinitions/Reference.yamlはどういう形式が良いだろう？

#### namespace部分を都度記述する形式

頑健なのは以下の表な形式。`definitions`部分をnamespaceと捉えて都度記述する。

Reference.yaml

```yaml
definitions:
  Reference:
    type: object
    properties
      $ref:
        type: string
    required:
      - $ref
```

これを参照する箇所では以下のような形で `$ref` を書くことになる。

main.yaml

```yaml
defintions:
  Reference:
    $ref: "definitions/Reference.yaml#/definitions/Reference"
```

以下のようなことを感じる。

- `$ref` の記述が冗長
- １つのファイルに複数の対象の定義を記述できる

#### namespace部分を省略する形式

namespace部分はファイルのディレクトリ階層とファイル名に現れる、なので記述するべきは中の定義のみという解釈もあるかもしれない。その場合は以下のような形式になる。

Reference.yaml

```yaml
type: object
properties
  $ref:
    type: string
required:
  - $ref
```

このときこれを利用する側では以下の様な形式で使われることになる。

main.yaml

```yaml
definitions:
  Reference:
    $ref: "definitions/Reference.yaml"
```

ぱっと見こちらのほうがシンプルで良さそうに見える。これは以下の様なことを意味している。

- ファイルの階層構造とnamespaceが密結合
- １つのファイルに１つの対象のみ定義を記述できる

### bundle後の表現について

先程の例で1つのファイルを複数のファイルに分ける行為について考えた(separate/separation)。今度はその逆の表現について考えてみる。これをbundleと呼ぶことにする。複数のファイルを１つにまとめる行為のこと。

つまり以下の様に使われる。

```console
# definitions/*.yamlは自動読み込まれるかもしれない
$ bundle main.yaml definitions/*.yaml > bundle.yaml
```

#### namespace部分を都度記述する形式

namespace部分を都度記述する形式の場合には何も考える必要がない。

main.yaml

```yaml
defintions:
  Reference:
    $ref: "definitions/Reference.yaml#/definitions/Reference"
```

`$ref` 部分にそのまま挿入されるだけ。

```yaml
definitions:
  Reference:
    <Reference.yamlの#/definitions/Referenceの中身が展開>
```

#### namespace部分を省略する形式

namespaceを省略した場合は少しむずかしい。展開先をどうするか考える必要がある。

例えばこのような記述があった場合には問題がないのだけれど(Reference.yamlの内容をそのまま展開すれば良い)。

```yaml
defintions:
  Reference:
    $ref: "definitions/Reference.yaml"
```

以下のように別のnamespace(ここではdefinitionsやresponsesの意味で使っている)から参照された場合について考えてみる。

```yaml
responses:
  DefaultResponse:
    properties:
      description:
        type: string
      schema:
        $ref: "#/definitions/Schema"  # <-
  Response2:
    properties:
      description:
        type: string
      schema:
        $ref: "#/definitions/Schema"  # <-

# 以下のどちらかになる
        # $ref: "./definitions/Schema.yaml"
        # $ref: "./definitions/Schema.yaml#/definitions/Schema"
```

こういうようなときに、できれば、 `#/definitions/Schema` になってほしい。ファイルの位置から `definitions/Schema` が算出できるなら大丈夫。そうでないなら妥協点を探さなければいけない。

たとえばきれいな出力を諦める。

```
responses:
  DefaultResponse:
    properties:
      description:
        type: string
      schema:
        <Reference.yamlの#/definitions/Referenceの中身が展開>
  Response2:
    properties:
      description:
        type: string
      schema:
        $ref: "#/responses/DefaultResponse/propeties/schema"
```

これはふつうに気持ち悪い(jsonpointer的にはOKなのだけれど)。

あるいは展開先のdefaultを決める(defaultを `#/definitions` に)。これは展開先のデフォルトが決まらない場合に不都合があったりする(例えばOpenAPI2.xとOpenAPI3.xを同時に対応しようと思った場合など)。そしてデフォルトの展開先以外に展開することができない(`#/responses`に展開など)
