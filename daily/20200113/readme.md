## python libcst

- https://github.com/Instagram/LibCST

```console 
$ pip install libcst
```

CSTをそのまま出力する

```console
$ python -m libcst.tool print <some_py_file.py>
```


### 利用例

tornadoのdecoratorベースのcoroutineをasync/awaitに書き換えている(native coroutineと呼ばれてる）

- https://chairnerd.seatgeek.com/refactoring-python-with-libcst/

## python pip upload error

twineでuplodしたときにいつもの

```
Content received from server:
<html>
 <head>
  <title>400 The description failed to render for 'text/markdown'. See https://pypi.org/help/#description-content-type 
for more information.</title>
 </head>
 <body>
  <h1>400 The description failed to render for 'text/markdown'. See https://pypi.org/help/#description-content-type for
 more information.</h1>
  The server could not comply with the request since it is either malformed or otherwise incorrect.<br/><br/>
The description failed to render for &#x27;text/markdown&#x27;. See https://pypi.org/help/#description-content-type for
 more information.


 </body>
</html>
HTTPError: 400 Client Error: The description failed to render for 'text/markdown'. See https://pypi.org/help/#description-content-type for more information. for url: https://upload.pypi.org/legacy/
```

### readme-renderer?

twine checkは通った。readme-rendeerを使っているはずだけれど。どうだっけ？

```console
$ pip install readme-renderer
$ pipdeptree -p readme-renderer
readme-renderer==24.0
  - bleach [required: >=2.1.0, installed: 3.1.0]
    - six [required: >=1.9.0, installed: 1.13.0]
    - webencodings [required: Any, installed: 0.5.1]
  - docutils [required: >=0.13.1, installed: 0.15.2]
  - Pygments [required: Any, installed: 2.5.2]
  - six [required: Any, installed: 1.13.0]
```

どうもこれはReSTのためだけのものっぽい。

### markdownは？

こうやってインストールしないとだめだった。extras_requires

```
pip install readme_renderer[md]
```

cmarkgfmか。

```console
$ python -m readme_renderer README.md 
<string>:16: (WARNING/2) Inline literal start-string without end-string.
```

なんでformatサポートしていないんだ。。

- https://github.com/pypa/readme_renderer/blob/master/readme_renderer/__main__.py

## webauthn

- https://github.com/duo-labs/py_webauthn

warehouceのrepositoryをのぞいてみつけた

- https://github.com/pypa/warehouse/blob/master/requirements/main.in

## 昨日のhandofcatsのupdateなどの記事のこと

結構コストは掛かったのだけれど。バグなどに気づいたのでふつうに良かった。
正確に言うとargparseのhelp formatterがsub parserには伝搬しないなどの細
かい話だけれど。

あとはふつうにmonogusaとhandofcatsの比較ができてよかった。今日はコード
生成周りについてテキトーにつぶやけたので頭の整理ができた。

handofcatsのAST周りに関しては深く立ち入ると厳しいなと言うことがわかっ
てきた。最悪真面目にやるとコードの削除はフロー解析が必要になる。その前
にグラフへの変換が必要になる。そしていろいろ考えてみるとlib2to3で進め
るよりはlibcst辺りを使っていった方が良いかもしれない。という気持ちになっ
た。lib2to3のpytree.Nodeは欠損はないけれどいろいろとふわっとしていてきつい。 

### もうちょっと思考の整理

注力しているのは以下の3つくらい

- handofcats (今週だけ)
- monogusa
- metashape

それぞれの未来などを考えたりしたいところ。どれも同じ未来のための試金石
というような感じなのだけれど。この辺りを良い感じでまとめておきたいとこ
ろ。

どれもeasy to useではあるのだけれど、いわゆるeasy to useではないという感じ。もう少し良い感じの解像度で捉えたいような感じ。手が届く。変換可能。構成管理ができる。あるところで動いたものが他の場所でも動く。みたいな感じ？もうちょっと良い感じにまとめたい。

なぜ既存のツールを使わないのか。構成管理やprovisioningと何が違うのか。わかっているのはインフラ用途ではなく開発用途に近いと言うような感覚。通常のツールのデプロイに対応する部分がアプリケーションコードの生成辺りになるのではないかというような仮説がある。このためのツール群という感じ。なのでインフラに対する構成管理のものは参考にはなるものの利用はできない。出力先がこれらのインフラ用の構成管理の設定ファイルなどにはなりうる。

CIとの連携や設定方法chatbotの運用辺りもこれは組み込みのときに使えるタスクという感じであってそれ自体ではない感じ。実際の適用事例としては参考になるというような。

モニタリングとかはどうなんだろう？この辺りもUI的には後でつけれるようにはしたいところ。ただ現状は伝搬していくイベントの連鎖のほうが大切。そしてステップを細かく分けられることが大切。いわゆるビルドツールでもタスクランナーでもないという辺りも大切な話かもしれない。これらを記述するための元となるもののresoruceと呼べば良いのかわからないがそのようなものの管理を良い感じにしたい。

エコシステムをまたいで良い感じに表現されてほしいという気持ちがあるのだよなー。この辺りも大切な話。

### もう少し考えたいこと

エコシステムをまたいだ感じというのは重要。あとはここのエコシステム間の接続のコミニュケーション経路の種類の数が爆発するののも避けたい。ただ汎用的なフォーマットを提供したいわけではない。この辺りも大切な話。

コードの生成を情報の伝播と考えたときに、parser,transformer,emitterくらいの粒度で分けられる。これらのうちparserをなくせないかと言うのがmetashape。実際にはすべての入力を（とりあえず）pythonのオブジェクトを仮定してコード変換と出力を主な機能としている。一種のインフラのようなもの。

monogusaは実験先や適用先のアプリケーションとしての利用を考えていたがマージする未来はまだ遠い。複雑なドメインを要求するものでなければあまりmetashapeのような機能は要求されない。一方でいろいろな場所で同じコアが動くという部分つまるところポート部分（外界と接するアダプター的な部分）の試行錯誤をやってみている感じ。現状commandとqueryのうちのcommand部分について注力している。

query部分やUI的なツールボックスをどうするかということに関しては現状課題として残っている。

あとなぜpythonを使っているかというところもどこかで説明したいかもしれない。重要なのは依存が切れるということ。依存が切れるというのは途中で読み込むことを止められるということ。そしてoptionalな型チェックが行えること。型情報が実行時に取得できること。最悪メタプログラミングでアレコレできること。という辺りを元に作業をしている。

通常、各コンポーネント間の参照は依存を招く。断ち切るには何らかの方法でendpointを切ってURLを共有するという形で通信可能にしなくてはいけない。この通信の設定ミスをなくしたい。ここの設定のペアを生成によって常に同期した形にするというのがコード生成の利点の一つ。もう一つは値のコピーによる値の伝搬。

同じ表現になりうるものをコピーによって伝搬させることで共通の定数パッケージというような大げさな依存を断ち切ることができそう。

handofcatsはなにかというと、実行時に扱う表現と生成に扱う表現の両立ができないかということを念頭にしているような気がする。実際のところこれは正確ではない。実装としてejectを要求するときに、重要なコア部分はコードとして共通の処理が通るという状況を作りたいと言うような気持ちの検証という面がある。あとはこちらは何らかの変換ではなく埋め込みとしてのコードセットのハンドリングというようなことを気にして実装している気がする。コードの断片をコンポーネントとして埋め込みたい。通常のモジュールそれ自体ではなく、それによって生成される結果に対して、現状のコードを直接触るときと同様の感覚でモジュールやコンポーネントという単位で扱えるようにしたい。

これは変換ではなく自由に記述することができる状態でのmetashapeの実験と考える事もできるかもしれない。

## いろいろな周辺の事柄の認識


わりとちょっとしたことのために時間を割く気質だったりするのだけれど。調整が必要がものと調整が不要なものとの違いは結構大きいのだよなー。
例えばシェルのワンライナーでおしまいにできるということなどはそれそのもの。

そして人の手が介在した瞬間にどうやっても一ステップ数分掛かる感じになる。
頑張れば10回くらい試すことはできそうだけれど。日常的に100回などはちょっと無理。

ejectの機能は常に最高で初手でやるべきか？というのはそうでものなくって。ejectするということはsnapshotを取るということで、実質forkなのだよなー。upstreamへの追随が辛くなる。

しかし一度きりしか使わないものを真面目に作り込むのは無駄ではという価値観もあるかも。
これに関してはどれになるかわからないmain streamとの距離を一定以上離したくないというような不安感への対応という感じかも。

## linter? fixer or code generator

あとlinterではなくfixerやcode generatorの方に興味がある理由もなんとなくわかってきて、あるコードベースに特定の形に沿った影響を与えたいのだけれど、コードのサイズが際限なく大きくなって来るとそれが辛くなってくるという感覚のせいかも。

code generatorは開発を高速化するためのものではなく品質を保つためのものなのだけれど。あんまり理解が得られない。あと失敗するとイラつく不快感の対象になったりもする。
特に依存がそのまま実行時間に影響するので最も先頭の依存を変えたときに無駄な待ち時間が発生するような形になると辛い。。

コードサイズと人の手の話の実施時間の話がある一方で、コードサイズが大きくなると複雑さも上がるので対応するfixerなどをまともに書けるのかという話も出てくる。(そういうこともあって(?)、lib2to3を使い回すよりはlibcstなどのほうが良いよなーと思ったりもしている)

メタプログラミングとコード生成やそれに類するものとの違いはステップを分割できるかどうかの違いな気がしている。メタプログラミングはその場で消費されてしまう。それが良い場合も悪い場合もあるけれど。

そうそうcode generatorのメタプログラミングに対する良い面もあって。それは依存の末端部分なら自由に手が届くので書き換えて無理やり対応が許される点。(ChatOpsのそれとは対照的な部分)

ちなみにメタプログラミングはそれがステップなどに分割されずに一気にやってくる（良い場合もある）まぁでも一般的にメタプログラミングの不意打ち的なバグは直しづらいことが多い。（依存が多くてあちらを立てればこちらが立たずみたいな形になりがち。緊急脱出ハッチを作りづらい）

## ChatOpsと属人性との関連

ちょっとだけ面白いなと思ったのは、ChatOpsは他の人に対してもopsの一部を公開しようという話なのだけれど。
一方でbot自身に関する属人性は上がるので、作り込み過ぎた場合には、インフラの知識がある人でも、隣の部署のインシデント時には祈るだけみたいな感じになる場合があることがあるというところ。（いや実際には祈るだけというほどではないけど。手足がもがれてオブザーバーに徹するしかないみたいな感じのほうが近いかも）

そういう意味ではcode generatorも同様の轍を踏むことになる可能性があるので慎重になったりもする。。

parserとtransformerというふうに考えるなら、既存のものを良くするfixer (translator)ということになるし。inputは手元にあるので適した表現で出力ということを考えるとcode generatorになる。これが一度きりだとscaffoldになる。

scaffoldは便利な足場を作るためのsnipetと考えると結構辛くって生産性が上がったんだか下がったんだかよくわかんない感じになったりする。
scaffoldが輝くのは複数の箇所への一貫性を持った参照を記述したくなるとき（例えばrouterのpathとcontroller名など）な気がしている。

という辺りをようやく最近言語化できる感じになってきた。

ビルド全体部分への理解は属人的になったりする（可能性がある）のは変わらずだけど。
