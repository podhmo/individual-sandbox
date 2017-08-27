"""
#[python]pythonでサブクラスを定義するときにオプションを渡せるようにしてみる

そういえば、継承時にbase classの他にオプションを取る定義が書けるのわりと最初ビックリするけれどvalidなpythonのコード。

3.6なら`__init_subclass__`のフックを使うのが楽かもしれない。

たとえば、以下の様なコードを書いてみる。

- 自身を継承したクラスをchildrenという変数に格納する
- 継承時にnameというオプションを与えた時にはかわりにその名前を格納する
"""
"""
## 3.6の場合

3.6以降の場合
"""


class A:
    children = set()

    def __init_subclass__(cls, name=None):
        name = name or cls.__name__
        cls.children.add(name)


class B(A):
    pass


class C(A, name="MaybeA"):
    pass


print(A.children)
"""
## 3.6以前の場合

メタクラスを使って対応することもできる。ただ自分自身を格納しないようにするのがちょっとトリッキー
"""


class AMeta(type):
    def __new__(self, clsname, bases, attrs, name=None):
        instance = super().__new__(self, clsname, bases, attrs)
        if "children" not in instance.__dict__:
            instance.children.add(name or clsname)
        return instance


class A(metaclass=AMeta):
    children = set()


class B(A):
    pass


class C(A, name="MaybeA"):
    pass


print(A.children)
