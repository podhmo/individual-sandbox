## python corountine

corountineというのはiteratorを取る代わりに何かの継続っぽいものを取るみたいな感じなのだなー。

```python
def run(itr):
    yield ["x", "x*x"]
    for x in itr:
        x = int(x)
        yield {"x": x, "x*x": x * x}
```

と

```python
def run():
    x = yield ["x", "x*x"]
    while True:
        x = int(x)
        x = yield {"x": x, "x*x": x * x}
```


## python jediで上手く補完できない

jediはこの環境変数を見る `VIRTUAL_ENV`。
つまり以下ではダメ。

```console
$ <venv>/bin/python use_jedi.py
```

これならOK

```
$ VIRTUAL_ENV=<venv> python use_jedi.py
```

もちろんこれも

```
$ . <venv>/bin/activate
$ python use_jedi.py
```

## python chatbot について考える

- https://github.com/lins05/slackbot

依存関係

```
slackbot==0.5.3
  - requests [required: >=2.4.0, installed: 2.18.4]
    - certifi [required: >=2017.4.17, installed: 2018.1.18]
    - chardet [required: >=3.0.2,<3.1.0, installed: 3.0.4]
    - idna [required: >=2.5,<2.7, installed: 2.6]
    - urllib3 [required: >=1.21.1,<1.23, installed: 1.22]
  - six [required: >=1.10.0, installed: 1.11.0]
  - slacker [required: >=0.9.50, installed: 0.9.65]
    - requests [required: >=2.2.1, installed: 2.18.4]
      - certifi [required: >=2017.4.17, installed: 2018.1.18]
      - chardet [required: >=3.0.2,<3.1.0, installed: 3.0.4]
      - idna [required: >=2.5,<2.7, installed: 2.6]
      - urllib3 [required: >=1.21.1,<1.23, installed: 1.22]
  - websocket-client [required: >=0.22.0,<=0.44.0, installed: 0.44.0]
    - six [required: Any, installed: 1.11.0]
```

基本的にはslackerに依存している？そんなことはないか。

あんまり好きじゃない点

- pluginsの渡し方
- 正規表現以外にroutingの方法はないのかな
- 動作確認が面倒
- 他の環境に持っていくことができなそう(e.g. discord)
- なぜrun.pyを自分で書かなければいけないのか

好感が持てる点

- 設定のとり方(環境変数を見たあとにおもむろに `from slack_settings import *`)

## python meshitsukaiの内容思い出してみる

slackerがなかった頃のやつ

- python-daemonってなんだっけ？
- なぜか頑なにrequestの構造を作ろうとしている
- routingどうやってたんだっけ？
- Configuratorなどが仰々しい
- テストのためにfakeとか作ってるみたいだけれど。上手く分離できていない
- yapsyみたいなpluginライブラリに頼ってしまってplygin自体を書くのが面倒になっている
- miniconfig作る前のやつだっけ？

```
meshitsukai==0.0
  - python-daemon [required: Any, installed: 2.1.2]
    - docutils [required: Any, installed: 0.14]
    - lockfile [required: >=0.10, installed: 0.12.2]
    - setuptools [required: Any, installed: 28.8.0]
  - requests [required: Any, installed: 2.19.1]
    - certifi [required: >=2017.4.17, installed: 2018.4.16]
    - chardet [required: >=3.0.2,<3.1.0, installed: 3.0.4]
    - idna [required: >=2.5,<2.8, installed: 2.7]
    - urllib3 [required: >=1.21.1,<1.24, installed: 1.23]
  - setuptools [required: Any, installed: 28.8.0]
  - slackclient [required: Any, installed: 1.2.1]
    - requests [required: >=2.11,<3.0a0, installed: 2.19.1]
      - certifi [required: >=2017.4.17, installed: 2018.4.16]
      - chardet [required: >=3.0.2,<3.1.0, installed: 3.0.4]
      - idna [required: >=2.5,<2.8, installed: 2.7]
      - urllib3 [required: >=1.21.1,<1.24, installed: 1.23]
    - six [required: >=1.10,<2.0a0, installed: 1.11.0]
    - websocket-client [required: >=0.35,<1.0a0, installed: 0.48.0]
      - six [required: Any, installed: 1.11.0]
  - websocket-client [required: Any, installed: 0.48.0]
    - six [required: Any, installed: 1.11.0]
  - yapsy [required: Any, installed: 1.11.223]
```
