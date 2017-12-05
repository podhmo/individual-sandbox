#[python][jinja2]悩んだ末に自分で自分の使うツールを作った話

この記事は[pythonアドベントカレンダーその2](https://qiita.com/advent-calendar/2017/python_python)の4日目の記事です。

## はじめに

今回は色々悩んだ結果自分で自分の使うツールを自作した話をしようと思います。

### 作ったものはjinja2のcli wrapper

作ったものはjinja2のcli wrapperです。[kamidana](https://github.com/podhmo/kamidana)と言います。
名前の由来は[jinja2が日本語の「神社」が由来なので](http://jinja.pocoo.org/docs/2.10/faq/#why-is-it-called-jinja)それをよりポータブルにというか持ち運びやすい感じになったものは「神棚」かなというような安直な感じです。

使いかたは以下の様な感じです。kamidanaにtemplateファイルを渡すと適用した結果を出力してくれます。普通ですね。

```console
$ cat hello.j2
hello. {{name}}
$ cat data.json
{
  "name": "foo"
}

# こういう形で利用する
$ kamidana hello.j2 --data data.json
hello. foo
```

## なんで新しいjinja2 wrapperを作ったの？

そもそもなんで新しいjinja2 wrapper作ったのかというと理由があります。

### どれが本物のupstreamかわからない

１つはjinja2のcli用のwrapperの実装がそこかしこに散在していてどれが本当のupstreamかわからないからです。pipでインストールできるだけで以下の様なものがあります。

- [j2cli](https://github.com/kolypto/j2cli)
- [j2cli3](https://github.com/jjzhangg/j2cli)
- [jinja2-cli](https://github.com/mattrobenolt/jinja2-cli)
- [jinja2-cli-whitespace](https://github.com/dlutsch/jinja2-clia)

そして一番star数の多いものは更新が止っているようでPRを出しても反応がなさそうでした。何か英語で頑張るよりも自分用のものをサクッと作ったほうが楽そうに見えたからです。

### 足りない機能があった

個人的には足りない機能が2つほどありました。言ってしまえばどのjinja2 wrapperもそこまで大したコード量でもないので位置から自分で作っても労力は大差ないだろうという思いもありました。ちなみにその足りない機能というのは以下の２つです。

- 個人的なヘルパー関数を定義してその場でだけ使いたい
- 複数の設定を同時に渡したい

もう少しどういう機能なのかを説明します。

#### 個人的なヘルパー関数を定義してその場でだけ使いたい

例えば先程の出力結果に現在時刻をちょこっと表示したいとしましょう。そのような時に簡単にテンプレートとして使える形で利用したいのです。ところが既存のjinja2のwrapperではそれを上手くサポートする方法がないようでした。

kamidanaの場合には`-a`(`--additionals`)オプションをつけることでそれができます。

```console
$ cat hello2.j2
hello. {{name}} (now={{now()}})

$ kamidana hello2.j2 -a ./now.py --data data.json
hello. foo (now=2017-12-04 21:40:18.210965)
```

このときnow.pyというファイルを新たに作っています。これは以下のようなdatetime.nowを呼ぶだけの単純なものです。この時作ったnow.pyは別途pipなどからインストールしたりimport可能な範囲に置く(例えばPYTHONPATHに追加する)などする必要がありません。これがやりたかったことの１つです。

now.py

```python
from kamidana import as_global
from datetime import datetime

@as_global
def now():
    return datetime.now()
```

as_globalのデコレーターをつけるとそれがテンプレート中でも利用可能になります。

ただ、今度はこの現在時刻を別の表現で出力したくなったりしませんか？そういうときにはjinja2のフィルターをその場で定義することもできます。now.pyに以下のような関数を追加します。今度はas_filterというデコレーターを付けます。

```python
from kamidana import as_filter

@as_filter
def datetimefmt(dt):
    return dt.strftime("%Y/%m/%d")
```

そしてテンプレート中でこれを利用します。

```console
$ cat hello3.j2
hello. {{name}} (now={{now()|asdate}})

$ kamidana hello3.j2 -a ./now.py --data data.json
hello. foo (now=2017/12/04)
```

これがやりたかったことです。たとえば、設定ファイルを生成したいと思います。そのような時に利用する設定値の表現がkebab-caseであったりsnake_caseであったりcamelCaseであったりと利用したい表現が様々になったりしていることがあります。それらを場所場所によって適切な表現に書き換えるための関数が利用可能になったテンプレートという形でjinja2を利用したかったのでした。

#### 複数の設定を同時に渡したい

もう１つほしかった機能として複数の設定を同時に渡したいというものがあります。これはどういうことかと言うと、元々マイクロサービス的なアプリケーションの設定ファイルの生成に利用できないかというところがjinja2のcli wrapperがほしいということの始まりだったのですが。そこで直行した設定に関しては別途わけて複数の組み合わせで表示したい。という思いがありました。

例えば、production,staging,testingの環境について、api用の設定がありまたdb用の設定があった時に基本的にはenv名(productionならproduction)付きのprefixがendpointでdbを作る。一方でapi用の設定はtestingとstagingは同じものdevで始まるものみたいな設定を行いたいとします。

以下の様な形です。

|env|db setting|api setting|
|:--|:--|:--|
|production|production.<db host>|https://api.foo.com|
|staging|staging.<db host>|https://dev-api.foo.com|
|testing|testing.<db host>|https://dev-api.foo.com|

この時複数の設定を同時に渡したいのですがそれをする口が用意されていませんでした。kamidanaではできるようにしました。

```console
# production
$ kamidana --data db/production.json --data api/production.json > conf/production.json
# staging
$ kamidana --data db/staging.json --data api/dev.json > conf/staging.json
# testing
$ kamidana --data db/testing.json --data api/dev.json > conf/testing.json
```

この時の設定がどうなっているかは `--dump-context` オプションをつけることで見ることができます。

```console
# 以下の２つの設定ファイルがある時に
$ cat db/production.json
{
  "db": {
    "url": "production.<db host>"
  }
}
$ cat api/production.json
{
  "endpoint": "https://api.foo.com"
}

# --dump-contextでその時に使われる設定がどのようなものかわかる
$ kamidana --data db/production.json --data api/production.json --dump-context
{
  "db": {
    "url": "production.<db host>"
  },
  "endpoint": "https://api.foo.com",
  "template_filename": "conf.j2"
}
```

#### 設定のオーバーライド

ところで複数の設定が行えると行った場合に設定のオーバーライドもしたくなるのではないでしょうか？例えばlocal用の設定がありそれに対して各自のユーザー用の設定を用意したいと言うような場合などがあると思います。もちろんできます。

例えば以下の様なローカル用の設定があったとします。
```console
$ cat conf.json
{
  "fooapi": {
    "endpoint": "https://foo.jp/api",
    "password": "this is api password",
    "username": "dev"
  }
}
```

このような設定のpasswordとusernameだけを書き換えたい場合やendpointを書き換えてproxyをはさみたいみたいな場合があったりすると思います。そのような場合には以下のように２つ重ねて渡すと後に渡した方の設定が優先されます。

```console
$ kamidana template.j2 --data conf.json --data local/me.json --dump-context
{
  "fooapi": {
    "endpoint": "https://foo.jp/api",
    "password": "*my password*",
    "username": "me"
  },
  "template_filename": "template.j2"
}
```

ちなみにリストはどうなるかと言うと追加はされずに上書きになります(注意してください)。

```console
$ echo '{"xs": [1,2,3]}' > num0.json
$ echo '{"xs": [1,3,5]}' > num1.json
$ kamidana --dump-context dummy.j2 --data num0.json --data num1.json --dump-context
{
  "xs": [
    1,
    3,
    5
  ],
  "template_filename": "dummy.j2"
}
```

#### ファイルにもしたくない場合には標準入力からでも

さらに一時的な用途の場合にはファイルも残したくない場合があると思います。そのような場合には標準入力を渡すことでも上書きを行えます。

```console
$ echo '{"fooapi": {"endpoint": "http://proxy.foo.jp/api"}}' | kamidana -i json template.j2 --data conf.json --dump-context
{
  "fooapi": {
    "endpoint": "http://proxy.foo.jp/api",
    "password": "this is api password",
    "username": "dev"
  },
  "template_filename": "template.j2"
}
```

### そもそもjinja2のcliを毎回コマンドとして呼ぶのは良いことなのかという話

そんなわけで少しずつ不満を解消するために自作のjinja2 wrapperを育てていたのですが、生成する設定ファイル数個ではなく数十個の単位になってきた時にちょっとした問題が発生しました。時間がかかりすぎるのです。調べてみるとほとんどはjinja2のimportの時間でした。

```console
$ time python -c 'import jinja2'
real    0m0.139s
user    0m0.109s
sys     0m0.030s
```

たとえば１つの設定ファイルを生成するのに0.1秒掛かるとします。それが４０個位になった場合には4秒かかります。1秒なら待っていられるのですが４秒では待っていたくありません(実際のところ最近jinja2のimportが早くなったのでこれは改善後の速さ。[その前はもう少し遅かった](https://gist.github.com/podhmo/6b72e17e20cd0b0d95d1887633b9b92b))。

そしてほとんどがjinja2のimport時間なのです。そんなわけでbatchモードも追加しました。以下の様な記述をすると一度の実行で複数のファイルが生成できるようになります。

例えば以下の様なtemplateを利用した設定ファイルの生成をしたいとすると(`--dst`はファイルに保存するオプション)

```console
$ echo '{"name": "foo"}' | kamidana -i json hello.j2 --dst out/foo.hello
$ echo '{"name": "bar"}' | kamidana -i json hello.j2 --dst out/bar.hello
$ kamidana hello.j2 --data me.json --dst out/me.hello
```

上のものと等価なバッチモードでの実行は以下の様になります(`kamidana-batch`という名前は正直微妙だと思います)。何が良いかと言うとほとんど実行時間が1/Nになることです(脇道にそれると、この種のimportの時間で作業がはかどらなくなる問題は取るに足らないスクリプトでの`import pandas`などでも発生しますね。解決策としてはreplを使うこと、つまるところjupyter notebookからpandasを使うことなどが挙げられます。moduleがimportされた状態を維持しておくという適応方法です)。

```console
$ cat batch.json
[
  {
    "data": {
      "name": "foo"
    },
    "outfile": "foo.hello",
    "template": "./hello.j2"
  },
  {
    "data": [
      {
        "name": "bar"
      }
    ],
    "outfile": "bar.hello",
    "template": "./hello.j2"
  },
  {
    "data": "me.json",
    "outfile": "me.hello",
    "template": "./hello.j2"
  }
]

$ kamidana-batch batch.json --outdir=out
```

### もちろんyamlなどにも対応してたりします

j2cliやjinja2cliの売りが複数フォーマットの対応のようなのでもちろんそれにも対応しています。

```console
# toml
$ kamidana template.j2 --data foo.toml
# yaml
$ kamidana template.j2 --data foo.yaml
```

ただし全部には対応はしておらず例えばjinja2cliはxmlに対応しているようですが対応していません。標準入力の受取には`-i <format>`オプションが必須です。標準入力はファイル名を持たないので拡張子からやってくるフォーマットを決めることができないので。

## 使いみち

例えばこのkamidanaの利用例としてreadme.rstの生成などをしていたりします。

たとえば、[kamidanaのreadme](https://github.com/podhmo/kamidana)はkamidanaで生成されています。コマンドの実行結果をreadme.rstに転写するのに便利でした。

実際readmeの生成は以下のようにして行われています。

```console
$ kamidana misc/readme.rst.jinja2 -a kamidana.additionals.reader > README.rst
```

readerにはファイルを読み込んだ結果を埋め込むためのフィルタやコマンドの実行結果を埋め込むためのフィルタが入っています。なので以下の様な形で使われています。

```
.. code-block:: console

{% with cmd="kamidana ../examples/readme/nginx.jinja2 --data ../examples/readme/data.json" %}
  $ {{cmd}}
  {{cmd|read_from_command|indent(2)}}
{% endwith %}
```

`read_from_command`はコマンドの実行結果を埋め込むフィルタで。これは以下の様な出力になります。

```rst
.. code-block:: console

  $ kamidana ../examples/readme/nginx.jinja2 --data ../examples/readme/data.json
  server {
    listen 80;
    server_name localhost;
  
    root /var/www/project;
    index index.htm;
  
    access_log /var/log/nginx/http.access.log combined;
    error_log  /var/log/nginx/http.error.log;
  }
```

markdownに慣れている方には見慣れない記法ですが、ReStructured Textを使ってしまったのでコードブロックの範囲はインデントをしないといけないのです。このあたりの調整が意外とめんどうなのですが。jinja2に乗っかってるおかげでindentのフィルタが付けられて便利です(indentはjinja2の組み込みのフィルタです)。

## さいごに

この記事では結局悩んだ末にjinja2のcli wrapperを新たにもう一つ作ったこと。そしてそれらで欲しかった機能について説明をしました。ただ本当に話したかったことは自分で車輪の再発明をしても良いんだよ。ということでした。

個人的な理想はいつの間にか現れていつの間にか消えてなくなるような空気みたいなパッケージでした(最終的に標準的な何かですべて賄うことができてそのときにはきれいになくなって消えている)。そして今までは結構頑なに継ぎ足しで既存のものを使うだったり標準から離れないということに拘泥していたところがあったのですが。それはそれとして常に既存のものから手を伸ばすというのが良いのかどうか考えるのも有用なことなんじゃないかと思ったりしました。

(例えばyamlが一時期流行ったりしましたが、多くの新しめの言語ではpackageの設定ファイルにはtomlを使うようになりyamlは捨てられる傾向にあるようです。既にyamlが存在しているからyamlをベースにと考えてしまうと抱え込んでしまった複雑性をそのまま開始地点として抱え込んでしまったりします。これはgoやrustの話。

一方で標準はtomlだからと読み込みにtomlだけをサポートした何かを作った場合には、過去のyamlのファイルをいちいち変換してまわるのかみたいな話があります。実際のところ、pythonにはlibyamlとそれのbindingのPyYAMLがあるのでyamlはそれなりに簡単に使えるというのに。)。

もちろん、標準から離れてメンテしなければいけないものを独自の実装にしているとあとで追随するのが難しかったりというのがあったりするので、あんまりアプリケーションのコアとなるようなライブラリに自作のものを使うのはオススメできません。あるいは独自の設計を内部の深い部分に残してしまうというのは手離れという意味ではやらないほうが良いという気持ちは今でもあります。一方で自分の日常の細々とした作業を行うようなツールに関しては自分の手先に合うものを自作してしまっても良いのではないかと思うようになりました(これがここ１年の心境の変化です)。

同じ話ではないですが、以下の２つの記事とスライドに共感的なものを覚えたりしました。

- http://watilde.hatenablog.com/entry/2017/09/26/180726
- https://www.slideshare.net/lestrrat/oss-77295079

まとめると「再発明は悪なのだけれど(再発明は悪じゃない)」みたいな感じの気持ちを表現しようとしたポエムでした。伝わる人がいたならうれしいです。
