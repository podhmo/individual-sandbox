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

## 追記

一旦戻って複数ファイルに対応するだけのコードを書いてみる。
それができたら存在するエラーを集める。

- FileNotFoundError (ref)
- KeyError (ref)
- yaml.scanner.ScannerError (syntax)

そういう意味ではpyyamlのエラーの内容を整理する必要もあるのか。

### 追記 pyyamlのerror

たぶんvalidation error以外は全部nodeが取れる？

```
yaml.scanner:ScannerError <- yaml.error:MarkedYAMLError <- yaml.error:YAMLError <- builtins:Exception <- builtins:BaseException <- builtins:object

yaml.error:MarkedYAMLError <- yaml.error:YAMLError <- builtins:Exception <- builtins:BaseException <- builtins:object
    [method, OVERRIDE] __init__(self, context=None, context_mark=None, problem=None, problem_mark=None, note=None)
    [method, OVERRIDE] __str__(self)

yaml.error:YAMLError <- builtins:Exception <- builtins:BaseException <- builtins:object
```

💭 本当は部分的に壊れても読み込みたいけれどsyntax errorてきなものはさすがに無理なような気がする。

エラーの一覧

```
class YAMLError(Exception):

class EmitterError(YAMLError):
class MarkedYAMLError(YAMLError):
class ReaderError(YAMLError):
class RepresenterError(YAMLError):
class ResolverError(YAMLError):
class SerializerError(YAMLError):

class ComposerError(MarkedYAMLError):
class ConstructorError(MarkedYAMLError):
class ParserError(MarkedYAMLError):
class ScannerError(MarkedYAMLError):
```

まぁMarkedYAMLErrorを取り出せれば良いような気がする？

## 追記

MarkedYAMLErrorのcontext_markとproblem_markが欲しい感じ。

```python
class Mark:

    def __init__(self, name, index, line, column, buffer, pointer):
        self.name = name
        self.index = index
        self.line = line
        self.column = column
        self.buffer = buffer
        self.pointer = pointer


class MarkedYAMLError(YAMLError):

    def __init__(self, context=None, context_mark=None,
            problem=None, problem_mark=None, note=None):
        self.context = context
        self.context_mark = context_mark
        self.problem = problem
        self.problem_mark = problem_mark
        self.note = note
```

### 追記

contextとproblemではproblemを優先したいかもerror messageは。どちらも表示するのが正しいかも？

```
problem could not find expected ':'   in "user.yaml", line 20, column 15
context while scanning a simple key   in "user.yaml", line 19, column 9
```

```
----------------------------------------
     1  components:
     2    schemas:
...
    12      UserNG:
    13        type: object
    14        properties:
    15          id:
    16            type: integer
    17          name:
    18            type: string
    19          contact_info
    20            $ref: './contact_info.yaml#/components/schemas/ContactInf'
```

## 追記

そう言えば、読み込んだ先のファイルでエラーになった部分のところはどういう表示が嬉しいんだろう？
少なくとも、`$ref`行にエラーが出て欲しい。理想としてはツールチップで全てのstackが見えて欲しい。

少なくともファイルの遷移のhistoryは欲しいかもしれない。

jsonknifeのbundlerを使う必要な無くてもう少しかんたんなもので良いはず？
expanderが必要そうだった。expanderのaccessorを見るのはめんどくさそうだな。。

## 追記

とりあえず繋げた。だるい。

## 追記

あとやらなくちゃいけない事は何だろう？

- 複数のエラーに対応
- lintでの表示に対応
- jsonschemaでのvalidationに対応
- shallow/deep validation
- remoteからresourceを取ってきたりupdateしたり
- pair部分の行位置を取得
- error対応

  - MarkedYAMLError

    - たぶんこのエラーはParserErrorなので処理が継続できない
  - KeyError

    - (NodeからMarkにする？)
    - valueに情報を持っていた(KeyError -> Value -> MapingNode -> ScalarNode)

  - validation error

### 追記

↑でのkeyErrorはReferenceErrorというとことかも。あとFileNotFoundErrorの場合がありそう。

ReferenceErrorに関してはfailfast的なオプションを持っていてその値ですぐraiseするかを決める。
ResoutionError辺りでWrapすると良い？（少なくともpythonに限って言うなら例外の体で集めるのが良い？）

### 追記

FileNotFoundErrorの場合も作る。

### 追記

厳密に言うと、以下がReferenceError ?

- KeyError かつ keyが $ref の valueを参照
- FileNotFoundError (これは絞らなくて良いでしょう)

### 追記

ReferenceErrorを追加した。
ちょっとStoreに機能をもたせた。以下の様な表示になってうるさい。

```
FileNotFoundError("[Errno 2] No such file or directory: 'cwd/example_linter/right.yaml' (where=cwd/example_linter/right.yaml) (where=cwd/example_linter/ng-pair.yaml)")
```

よく考えたらWrappedErrorみたいなものを用意するべきかも。。

https://github.com/podhmo/dictknife/issues/165

## 追記

次はlint表示をどうにかしよう。
💭 テキトーにLTSVあたりで出力することを考えようかな。

## 追記

とりあえずそれっぽい感じで表示するようにしてみた。


```console
$ python parse.py ng-main.yaml
status:Error    cls:ParseError  filename:ng-main.yaml   start:4@12      end:4@53     msg:could not find expected ':' (while scanning a simple key)    where:('ng-main.yaml', 'ng-user.yaml')
----------------------------------------
     1  components:
     2    schemas:
     3      User:
     4        $ref: "./ng-user.yaml#/components/schemas/User"
```

```console
$ python parse.py a1.yaml
status:Error    cls:ReferenceError      filename:a1.yaml        start:6@12      end:6@43      msg:ExFileNotFoundError(2, 'No such file or directory') where:('a2.yaml', 'b.yaml')
----------------------------------------
     1  components:
     2    schemas:
     3      a:
     4        $ref: "a2.yaml#/components/schemas/a"
     5      b:
     6        $ref: "a2.yaml#/components/schemas/b"
```

```console
$ python parse.py a2.yaml
status:Error    cls:ReferenceError      filename:a2.yaml        start:6@12      end:6@42      msg:ExFileNotFoundError(2, 'No such file or directory') where:('a2.yaml', 'b.yaml')
----------------------------------------
     1  components:
     2    schemas:
     3      a:
     4        $ref: "a3.yaml#/components/schemas/a"
     5      b:
     6        $ref: "b.yaml#/components/schemas/b"
```

```console
$ python parse.py ng-pair.yaml
status:Error    cls:ReferenceError      filename:ng-pair.yaml   start:7@16      end:7@43      msg:ExKeyError('/components/schemas/Left')      where:('ng-pair.yaml',)
status:Error    cls:ReferenceError      filename:ng-pair.yaml   start:9@16      end:9@54      msg:ExFileNotFoundError(2, 'No such file or directory') where:('ng-pair.yaml', 'right.yaml')
----------------------------------------
     1  components:
     2    schemas:
     3      Pair:
     4        type: object
     5        properties:
     6          left:
     7            $ref: "#/components/schemas/Left" # key error
     8          right:
     9            $ref: "right.yaml#/components/schemas/Right" # file not found error
```

## 追記

次どの辺りをしようかな

- コードをキレイに
- status codeを良い感じに決める
- validation errorの追加
- 複数行エラーをtooltipで表示（ちょっとこのlinterから離れる)
