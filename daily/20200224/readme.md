## python + wasm?

- https://github.com/brython-dev/brython (not wasm)
- https://github.com/bogdandm/json2python-models (if this module is running in frontend)

## monogusa 

- events
- cont

## python dataclasses

以下のどちらが良いんだろう？

- dataclassesをwalkして出力する
- dataclassをasdictしてdictをwalkする

asdictがinnerで隠されていたりするのがだるい。
一方でmetadataなどの情報を利用できるのが利点。

## python emit

複数の形式にemitしたい場合

- あるフォーマットAのrequired,optional
- あるフォーマットBのrequired,optional

これが食い違う事がある。そのあたりが大変かも？

### 考えてみると

考えてみるとdataclassをloopしたいだけでは？


## python metashape sqs

sqsのterraformのファイルを生成してみる

- https://www.terraform.io/docs/providers/aws/r/sqs_queue.html
- https://qiita.com/hayaosato/items/381ee4b5d657206de1e9

これはもしかしたら単にcookiecutter案件かもしれないけれど。

### teraform

importできたりもするのか。

- https://www.terraform.io/docs/import/index.html

### そういえば

そういえば知りたいものは各種設定ではなくて単純にterraformの設定ファイルのsyntaxでは？

- https://www.terraform.io/docs/configuration/syntax.html

関数っぽいのは何なんだろう？

```
  redrive_policy            = jsonencode({
    deadLetterTargetArn = aws_sqs_queue.terraform_queue_deadletter.arn
    maxReceiveCount     = 4
  })
```

ふつうに関数だった

- https://www.terraform.io/docs/configuration/functions/jsonencode.html

この記事が用語をざっくり知るのに便利そう。

- https://qiita.com/minamijoyo/items/1f57c62bed781ab8f4d7

#### formatter

```
$ terraform format
```

#### repl

```
$ terraform console
```

#### template_file

これだけで一種のtemplateを定義して、そこに引数として値を注入して、それを利用できる。

## python prestring

そういえばどれくらいで一区切りにしたいんだろう？
codeobjectが落ち着いたら一段落という感じにしたいかも。

後でgoについての修正を入れたかったりはする。
ただそれを行う前に一度リリースしちゃいたいかも。

### 気になるところ

goのpointerもそうだなーと思ったけれど。pythonのデコレータ部分も少しだるかった。これはcodeobjectで解決できないかもしれない。まぁ、関数としてちょちょっと定義してあげるのも悪くはないのかもしれない？

あと、returnにawaitが欲しくなったというのと同様にstmtにawaitが欲しくなることってあるんだろうか。
そしてそれを達成した場合には代入部分をawaitで包めなくなる。これは結局letがいつでも常に欲しくなるということなんだろうか？

あとは引数部分を名前にしてしまっているのが厳しい。これは引数にSymbolを渡してあげれば良いのかもしれない。

### ただの夢

最近の感覚としては

## monogusa, handofcats, metashape

monogusaとhandofcatsがprestringを利用している実コードという感じ。
ちょっとmoogusaのコードを書き換えようとしてみたけれど、参照関係が結構テキトーな感じになっているなーという感じ。
これは参照関係を意識した記述が難しいという話なのか、単に微妙なコードなのかという辺りが悩ましいところ。

とはいえ１つわかったことがあってcodeobjectの戻り値がおかしなことになっているのかもしれない。
ちょっとした思いつきでは戻り値としてSymbolを返すのが良いのではないか？と思った。ということは関数単位で返すべきなのだけれど。
トップレベルの定数やグローバル変数部分の参照をどう扱うべきかというところに悩みが出てきてしまっていた。これらを戻り値で返すのはなかなかに厳しい。いっその事常にdataclassを定義してみるというのはどうなんだろうなー。


