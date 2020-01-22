## 

## python type hints

typeshedの情報ってどうやってアクセスするんだろ？
jediは読み込んでるんだよなー。

この辺？

https://github.com/davidhalter/jedi/blob/master/test/test_inference/test_gradual/test_typeshed.py#L14

ふつうにmypyもjediもtypeshedを持っているっぽい。

### pep561 ?

- https://www.python.org/dev/peps/pep-0561/#type-checker-module-resolution-order

```
1. Stubs or Python source manually put in the beginning of the path. Type checkers SHOULD provide this to allow the user complete control of which stubs to use, and to patch broken stubs/inline types from packages. In mypy the $MYPYPATH environment variable can be used for this.
2. User code - the files the type checker is running on.
3. Stub packages - these packages SHOULD supersede any installed inline package. They can be found at foopkg-stubs for package foopkg.
4. Inline packages - if there is nothing overriding the installed package, and it opts into type checking, inline types SHOULD be used.
5. Typeshed (if used) - Provides the stdlib types and several third party libraries.
```

hmm

```console
$ python -c 'import site; print(site.getsitepackages())'
['VENV/lib/python3.8/site-packages']
```



## go テストを書くためにmockを書くのもinterfaceを書くのもめんどくさい

type aliasを使って同一パッケージ上にimportしてくる。
そしてテスト中はmockの方にビルドタグで切り替える。

https://www.kaoriya.net/blog/2020/01/20/never-interface-only-for-tests/

もう一つ方法があるかも。interfaceにするのを遅延しておく。

