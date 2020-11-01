## python starlette

意外とstarletteを書いてて便利に感じる。
ところで、method毎のendpoint定義をどうすればよいか？ということに少し悩んだりした。
調べてみるとHTTPEndpointというクラスが用意されていて、これがいわゆるclass base viewのように働く。

そういえば、というところでちょっとしたpandasの実行をするようなコードを書いたりした。
結果としてはファイル名が返ってくれば十分なので、すべてをbackground taskにしてしまおう。background taskは便利だ。まぁ、真面目に考えるとtaskの実行中で死んだら死ぬとかそれならキューに一度enqueueする？とかprocess事態のCPU消費量は大丈夫か？とか考えたりすることはでてくるが。簡単なものに関しては便利。

そして、結局、request/responseのvalidationがしたくなるよね。。というところに始まりfastAPIにたどり着いてしまう。。


### spawn_with_connection

egoistの spawn_with_connection を使ってみた。
たしかに早くはなる。しかしこのインターフェイスが気に入らないな。
withを使いたいし、waitを持っていて欲しい。

そしてこのバージョンのegoistはまだuploadしていなかったのか。
あと、extという名前は嫌だな。。

### そして

そして、なぜ1processで書かないのか？
コードと異なるのは何なんだろう？
毎回記述対象が増えていくのが嫌？現状はCLIの引数に日付を渡して日毎のバッチが動くようなイメージ。
引数として渡されるなら関数として定義されていても良い気がする。

あと、なんか壊れると繋ぎっぱなしになる？

## metashape pytho3.9

- t.get_args()にinclude_extrasのオプションが追加されていた
- _evaluate() missing 1 required positional argument: 'recursive_guard'
- listとt.Listは別物

## metashape fix typing

lru_cacheのエラーがだるい。こんな感じで迂回する。 `_typeinfo_evil.pyi` に迂回したいものを書く。

```
typeinfo.py
_typeinfo.py
_typeinfo_evil.py
_typinfo_evil.pyi
```

https://www.python.org/dev/peps/pep-0561/

>  Thus type checkers MUST maintain the normal resolution order of checking *.pyi before *.py files.

というわけで大丈夫そう。
