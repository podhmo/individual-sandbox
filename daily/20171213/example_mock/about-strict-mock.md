#[memo][python][mock]メソッドを置き換えたmockをもう少しstrictにしてみたい

メソッドを置き換えたmockをもう少しstrictにしてみたい。mockのpatchなどでobjectを置き換える時に属性の存在まではspecやspec_setで対応できるのだけれど。メソッドのsignatureまで含めて置き換え前のものと同じかどうか確認したい。

例えば、存在しない属性へのアクセスはエラーになる(これは期待通り)

```python
class Ob:
    def hello(self, name):
        return "hello:{}".format(name)


class Tests(unittest.TestCase):
    def test_attr_missing(self):
        # 属性無しはOK
        m = mock.Mock(spec_set=Ob)
        with self.assertRaises(AttributeError):
            m.bye()
```

一方でこれはダメ、元のメソッドから見たら引数不足なものの、mockはそれを関知しない(これは期待通りではない)

```python
class Tests(unittest.TestCase):
    def test_mismatch_signature(self):
        m = mock.Mock(spec_set=Ob)
        m.hello.side_effect = lambda: "*replaced*"

        # Ob.hello()から見たら不正な呼び出しなのだけれど。置き換えたmockとは合っているので通ってしまう
        got = m.hello()
        self.assertEqual(got, "*replaced*")
```

本来であればTypeErrorなどが発生してほしい。

```python
Ob().hello()
# TypeError: hello() missing 1 required positional argument: 'name'
```

## 現状のwork-around

雑に `replace_method_with_signature_check()` という名前の関数を定義している。

これを使うと以下の様にAssertionErrorが出るようになる。

```python
class Tests(unittest.TestCase):
    def test_mismatch_signature(self):
        m = mock.Mock(spec_set=Ob)

        # Ob.hello()に対して引数が不足した定義
        def hello():
            return "*replaced*"

        replace_method_with_signature_check(m, hello)

        got = m.hello()
        self.assertEqual(got, "*replaced*")

# AssertionError: expected hello()'s signature: (name), but ()
```

ちゃんとsignatureを考慮して見てくれる。もちろん、まともなsignatureの合った定義に書き換えたら呼び出し側の引数の不一致がわかり`TypeError`になる。

一応mockじゃないものに利用してしまった場合の事も考慮して `type(m)` と `m.__class__` を比較している(mockとmock以外を見分けるイディオム)。

## 実装

実装は以下の様な感じ。

```python
import inspect

def replace_method_with_signature_check(m, fn, name=None):
    """mock中のmethodをsignatureを考慮して書き換えるもの"""
    spec = m.__class__
    typ = type(m)
    name = name or fn.__name__

    assert typ != spec, "{} == {}, maybe spec is not set?".format(typ, spec)

    sig_repr = str(inspect.signature(getattr(spec, name)))
    sig_repr = sig_repr.replace('(self, ', '(')  # xxx work-around
    fn_sig_repr = str(inspect.signature(fn))
    assert sig_repr == fn_sig_repr, "expected {}()'s signature: {}, but {}".format(name, sig_repr, fn_sig_repr)
    attr = getattr(m, name)
    attr.side_effect = fn
```

微妙な点も残っていて、メソッドの置き換えを考慮するのに、self部分をカットする部分がすごく雑。これは `isnpect.signature()` で取れる値の引数部分が変更不可能なせいでもあるのだけれど。本当に真面目に頑張るのなら`inspect.getfullargspec()`の方を利用した方が良いかもしれない。


## 置き換えをオブジェクトで

もうちょっと不格好じゃない形で置き換えをしたい場合にはオブジェクトにしたほうが良いのかもしれない。

```python
class MethodReplacer:
    def __init__(self, m):
        self.m = m

    def __getattr__(self, name):
        return partial(replace_method_with_signature_check, self.m, name=name)


m = mock.Mock(spec_set=Ob)
rep = MethodReplacer(m)
rep.hello(lambda name: "*replaced*")
```
