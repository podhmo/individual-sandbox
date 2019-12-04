from prestring.python import Module

# 何が嫌だったのかを整理してみるか。

m = Module()

# こういう感じでクラス定義をするんだけれど。このクラスを利用する関数を書きづらい。
with m.class_("Person"):
    m.stmt("name: str")

# ここで "Person" って何？ importされているの？と感じてしまう。
with m.def_("print_name", "p: Person"):
    m.stmt("print(p.name)")

# そしてここで定義したprint_name()も利用できない。何がおきているかと言うと。
#
# - "Person"というクラス定義を生成する記述は値ではないので持ち運べない
# - "print_name"という関数定義を生成する記述は値ではないので持ち運べない
#
# どれも値として扱えないことが問題？ 例えば以下の様に関数で包むのはどうだろう。


def Person(m: Module) -> None:
    with m.class_("Person"):
        m.stmt("name: str")


# 何が嫌なのかと言えば、関数名とクラス名を二度書かないといけないと感じる点。
# ただ今度は値として持つことができる。
# 次にやりたいことはなんだろう？その値を使ってのコードとはなんのことだろう？
#
# - 型として使うこと
# - ふつうに実行すること
#
# これらはどうなってほしいのか？
# 素直に関数定義の記述もwrapしてみると以下の様になる。


def print_name(m: Module):
    with m.def_("print_name", "p: Person", return_type="None"):
        m.stmt("print(p.name)")


# wrapしたら呼び出してあげないといけない。呼び出してあげる自明な記述がない。

m = Module()
Person(m)
print_name(m)
print(m)

# 呼び出してあげるときのコードは何か違う。やっぱり実行を自分で書くのではない。
# 定義とimportによって自明に出力されるべきな気がする。


# もう１つ忘れていたことがあった。素直に名前空間をそのまま使いたい。
# つまりここに定義されたら__main__だし。
# importされたらその時の__module__なんだよ。。

# どうすれば良いんだろう？
# デコレータを付けていくのはバカバカしくないか？
# つまりやっぱり実行であって欲しいわけ。

