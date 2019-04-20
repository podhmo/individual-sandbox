## FRで遊ぶ

以下のことを気にしよう

- npmを省略(なるべく)
- cssを省略(style)
- UI部分で遊ぶ
- Layoutを気にする

### どういうものを書く？

欲しそうなUIの一覧になってほしい。storybook?ちょっと違う。

- progress bar
- spinner
- 一覧表示


### 便利そうなリンク

省略用

- https://oxal.org/projects/sakura/
- https://unpkg.com/

UI(課題的な話)

- https://sbfl.net/blog/2019/02/20/react-only-tutorial/
- https://github.com/tornadoweb/tornado/tree/master/demos/chat
- (Flutterあたりを見ても良いのでは？)

layout

- https://www.checklist.design/
- https://hashrock.github.io/solved-by-flexbox-ja/demos/holy-grail/
- https://coliss.com/articles/build-websites/operation/css/new-css-logical-properties.html


## memo

- 日記

## python master branchなどを指定してinstallしたpackageのupdate

- `--upgrade` を付けないとversionを満たしているときにはinstallしない
- `--force-reinstall` で古いversionであっても再installできる
- `--pre` を使ってpre-releaseのpackageをinstallできる

```console
$ mkdir /tmp/foo
$ cd /tmp
$ python -m venv foo
$ cd foo
$ ./bin/pip install "git+ssh://git@github.com/marshmallow-code/marshmallow.git@dev#egg=marshmallow"
$ ./bin/pip freeze | grep marshmallow
marshmallow==3.0.0rc5
$ ./bin/pip search marshmallow | grep "^marshmallow "
marshmallow (2.19.2)                        - A lightweight library for converting complex datatypes to and from native Python datatypes.
$ ./bin/pip install --upgrade marshmallow
Requirement already up-to-date: marshmallow in ./lib/python3.7/site-packages (3.0.0rc5)
$ ./bin/pip install --force-reinstall marshmallow
Collecting marshmallow
  Downloading https://files.pythonhosted.org/packages/bc/b4/f54f8533aad0645431bfda2cc3d20913a305779c80f8cb71229e8b615c5b/marshmallow-2.19.2-py2.py3-none-any.whl (50kB)
    100% |████████████████████████████████| 51kB 4.8MB/s 
Installing collected packages: marshmallow
  Found existing installation: marshmallow 3.0.0rc5
    Uninstalling marshmallow-3.0.0rc5:
      Successfully uninstalled marshmallow-3.0.0rc5
Successfully installed marshmallow-2.19.2
$ ./bin/pip freeze | grep marshmallow
marshmallow==2.19.2
$ ./bin/pip install --pre --upgrade marshmallow
Collecting marshmallow
  Downloading https://files.pythonhosted.org/packages/fd/91/06f9a52d8647fc7bebf24e3745f8eea4e2495006676353d3494d2f1afa93/marshmallow-3.0.0rc5-py2.py3-none-any.whl (44kB)
    100% |████████████████████████████████| 51kB 5.8MB/s 
Installing collected packages: marshmallow
  Found existing installation: marshmallow 2.19.2
    Uninstalling marshmallow-2.19.2:
      Successfully uninstalled marshmallow-2.19.2
Successfully installed marshmallow-3.0.0rc5
```

### `Removed build tracker '/tmp/pip-req-tracker-xbtw1gca'`

これを消さずに残したい。downloadを一回だけにしたい。(buildに失敗)

`--no-clean` ? 名前が違うけれど `/tmp/pip-install-**` には残る？

```console
$ ./bin/pip install mecab-python
$ ./bin/pip install mecab-python
Collecting mecab-python
  Using cached https://files.pythonhosted.org/packages/86/e7/bfeba61fb1c5d1ddcd92bc9b9502f99f80bf71a03429a2b31218fc2d4da2/mecab-python-0.996.tar.gz
```

ディレクトリの分散は最初の５文字をdirectoryに分けているのか。

```console
$ --cache-dir=x
```

### `--upgrade-strategy==only-if-needed`

### 特定のbranchを指定してinstall

```
git+<url>@<branch>#egg=<name>
```

例

```
git+ssh://git@github.com/marshmallow-code/marshmallow.git@dev#egg=marshmallow
git+https://github.com/marhshmallow-code/marshmallow.git@dev#egg=marshmallow
```

#### 参考

- https://pip.pypa.io/en/stable/user_guide/#requirements-files
- https://packaging.python.org/tutorials/installing-packages/
- https://pip.pypa.io/en/stable/reference/pip_install/?highlight=subdirectory#vcs-support

## LSP specをswaggerのdocumentに

- markdown parser
- typescript parser
- (typescript -> swagger)

- https://github.com/Microsoft/language-server-protocol
- https://github.com/Microsoft/language-server-protocol/raw/master/versions/protocol-2-x.md
- https://github.com/Microsoft/TypeScript/wiki/Using-the-Compiler-API
- https://github.com/Microsoft/TypeScript-Babel-Starter
- https://babeljs.io/docs/en/babel-plugin-syntax-typescript
- https://github.com/YousefED/typescript-json-schema
- https://github.com/quicktype/quicktype

## ioloopの実装

nestした場合ってどうなっているんだろう？

あー、wrapした戻り値がyieldしてadd_done_callbackで動き出すのか。

### IOLoop

- call_later
- call_at
- run_in_executor
- run_sync

futureを作っていい感じにアレコレする。

thread以外でやっている方法。callAtとかで値を見つつbusy loop。

### tornado.gen

- corountine
- Runner
- sleep
  - call_later

Runnerの中でcorountineを使う。

gen.corountine

- future作って、最後future_set_resesult_unless_cancelled(fut,result)
- 戻り値がGeneratorだったら、next実行する
- StopIterationだったらおしまい
- そうじゃなかったら、Runnerでwrapしてadd_done_callback()

### runner

```
tornado.gen:Runner <- builtins:object
    [method, OVERRIDE] __init__(self, gen: 'Generator[_Yieldable, Any, _T]', result_future: 'Future[_T]', first_yielded: Union[NoneType, Awaitable, List[Awaitable], Dict[Any, Awaitable], concurrent.futures._base.Future]) -> None
        [method] handle_yield(self, yielded: Union[NoneType, Awaitable, List[Awaitable], Dict[Any, Awaitable], concurrent.futures._base.Future]) -> bool
            [method] run(self) -> None
        [method] run(self) -> None
            [method] handle_yield(self, yielded: Union[NoneType, Awaitable, List[Awaitable], Dict[Any, Awaitable], concurrent.futures._base.Future]) -> bool
    [method] handle_exception(self, typ: Type[Exception], value: Exception, tb: traceback) -> bool
        [method] run(self) -> None
            [method] handle_yield(self, yielded: Union[NoneType, Awaitable, List[Awaitable], Dict[Any, Awaitable], concurrent.futures._base.Future]) -> bool
```
