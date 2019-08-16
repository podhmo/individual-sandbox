# yaml用のlinter

linterという言葉は適切ではないかも？正確に言うとvalidationも含むのでもう少し広い範囲なのだけれど。まぁ文法にだけという意味合いは特に無いからlinterで良いのかな。

## usecase?

最初に念頭に置いていたユースケースはopenapi doc。特にファイルが分割されたopenapi doc。
(コメントか何かでjsonschema(?)を参照するようにしてあげれば他の設定ファイルにも対応できるかもしれない)

## エラーの種類

エラーの種類がある気がする。phaseと呼んでも良いかもしれない。

- SyntaxError (ParseError)
- ReferenceError (LoadError)
- ValidationError

この内syntax errorは既存のlinterがサポートしているのだけれど。reference errorが結構な割合かもと言うのと、あわよくばValidationErrorも加えたいという感じ。

## エディタ上でのエラー表示に必要な情報

エディタからエラーを良い感じで表示させるにはエラーに以下の情報が含まれていると良い感じ。

- filename
- line number
- (position)
- summary
- (warning level)
- (description)

理想としてはposition(行内のどの位置か）まで気にしたいけれどとりあえずは行番号が分かれば。
あとはsummaryだけを見れば分かるのが理想ではあるけれど、そしてtooltipなどでdescriptionが見えれば最高だけれど、とりあえずはどの位置に不備があるかだけでも分かれば快適になりそう。

### 行番号の取得方法について

ちょっと面倒なのは行番号は読み込んだファイルそのものの表現上の行番号である必要があるという所。なのでロード後のデータとの対応関係が記録できている必要がある。

この辺りは以前調べた記憶。YAMLのparse時の状態をまるっと保持しておけばどうにかなった記憶。JSONはまぁYAMLのサブセットなのでそこはYAMLのものを使うことにしよう。

このあたり

- https://github.com/podhmo/individual-sandbox/blob/master/daily/20190515/example_loading/07.md
- https://github.com/podhmo/individual-sandbox/blob/master/daily/20190515/example_loading/07loadyaml.py

## 追記

めんどうなので(そしてgoの方は未対応が多そうというのと順序関係の保持に考えることが色々ありそうなのと慣れているので)pythonで書くことにしちゃおうかな。

そう言えばyamlのpackage２つあった記憶。

- https://pypi.org/project/PyYAML/
- https://pypi.org/project/ruamel.yaml/

余計なことをしなそうなのとstableなのでpyyamlにしようかな。

## 追記 行位置を覚えておくコードの整理

やっていくか。とりあえず以前に行位置を覚えておくコードをキレイにしよう。闇雲にadaptしていた記憶。

テキトーなテストデータも欲しいかも。。

### 追記

load時の処理自体も分解していたのか。結構調整しないとダメかも。
あと、dict,listだけ位置が存在していれば十分？めんどくさいのは最終的にloadされた後のデータとの参照を持たないといけない点。そしてpythonのidをkeyに取得する処理は地味に怖いかも。

### 追記

とりあえず何も考えずに行の位置を覚える部分をきれいにしていこう。
その前にどういう風に呼ばれているかを可視化しておこう。

document,mapping,sequenceだけをwrapすれば良い気がする。

```
wrap  construct_object
wrap         construct_scalar
wrap      construct_object
wrap         construct_scalar
wrap      construct_object
wrap         construct_scalar
wrap      construct_object
wrap         construct_scalar
wrap      construct_object
wrap         construct_scalar
wrap      construct_object
wrap      construct_object
wrap   construct_mapping
wrap      construct_object
wrap      construct_object
wrap   construct_sequence
wrap         construct_scalar
wrap      construct_object
wrap         construct_scalar
wrap      construct_object
wrap         construct_scalar
wrap      construct_object
wrap         construct_scalar
wrap      construct_object
wrap   construct_mapping
wrap         construct_scalar
wrap      construct_object
wrap         construct_scalar
wrap      construct_object
wrap         construct_scalar
wrap      construct_object
wrap         construct_scalar
wrap      construct_object
wrap   construct_mapping
wrap construct_document
```

### 追記

とりあえず必要なメソッドのwrapは判明した。特別な記述への対応を減らすだけでなくBaseLoader辺りを継承するようにしたほうが良いかもしれない？

## 追記 分割して呼び出していた部分をyaml.load()に合わせる

loaderをLoaderとして使えるようにインターフェイスを揃えるとかしたほうが良さそう。

💭 Reader,Scanner,Parser,Composer,Constructorの意味を忘れてしまっているな。。

### 追記

なんで大変になっているかと言うとdisponseが呼ばれてしまうとnodeが消えてしまうせい？
あー、あとload()の時点でdisponseが呼ばれちゃうな。

### 追記

とりあえずコードをシンプルにした。実行時のpathが状態として欲しくなる気がするのだけれど一旦消した。
今度は複数ファイルの読み込みの対応するのが良いかも(`$ref` のサポート)。
