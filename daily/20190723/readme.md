## emacs

以下が便利。

- toggle-debug-on-quit -- C-g で backtrace表示
- toggle-debug-on-error

## emacs python

時折めちゃくちゃおもくなることがある？


## python lazy loader

- importlib.util:LazyLoader の意義

## python loader 実は良いこと言っている

https://docs.python.org/ja/3/reference/import.html#replacing-the-standard-import-system

- あるパッケージからのimportを制御するのは `__import__()` の置き換え
- 全体を置き換えるには `sys.meta_path` を全部消す
- 特定のmoduleのimportを防ぐには `find_spec()` がNoneを返す代わりにModuleNotFoundErrorを送出する(Noneは他のfinderへの移譲を意味する)

## pythonのloaderとかfinder

- https://docs.python.org/ja/3/reference/import.html#finders-and-loaders
- http://www.robots.ox.ac.uk/~bradley/blog/2017/12/loader-finder-python.html
- https://www.python.org/dev/peps/pep-0302/
- https://www.python.org/dev/peps/pep-0451/
- https://github.com/bradleygramhansen/pyfo

### memo

- Finder
- Loader
- Importer = Finder + Loader

see: [importlilb.abc](https://docs.python.org/3/library/importlib.html#module-importlib.abc)

```
importlib.abc:Finder <- builtins:object
    [method] find_module(self, fullname, path=None)

importlib.abc:Loader <- builtins:object
    [method] create_module(self, spec)
    [method] load_module(self, fullname) # deprecated
    [method] module_repr(self, module) # deprecated
```

modern

```
importlib.abc:Finder2 <- builtins:object
    [method] find_spec(self, fullname, path=None, target=None)

importlib.abc:Loader2 <- builtins:object
    [method] create_module(self, spec)
    [method] exec_module(self, module)
```

`find_spec()` 自体はabcの中に含まれないのか。なるほど。

```
class MetaPathFinder(Finder):

    """Abstract base class for import finders on sys.meta_path."""

    # We don't define find_spec() here since that would break
    # hasattr checks we do to support backward compatibility.
```

知りたいのは `find_spec()` の型など。なのでtypeshedで調べるのが手軽かもしれない。

- https://github.com/python/typeshed/blob/efb67946f88494e5e9a273d59a3100a56514b15f/stdlib/3/importlib/machinery.pyi#L19-L22

というかこのpepか

- https://www.python.org/dev/peps/pep-0451/

### 全体の流れ

あとで丁寧に書くけど

```python
spec = find_spec(fullname, path, target)
module = spec.loader.create_module()
spec.loader.exec_module(module)
```

くらいの感じ。もう少し詳しい説明はこのあたりに。

- https://docs.python.org/ja/3/library/importlib.html#approximating-importlib-import-module
- https://docs.python.org/ja/3/reference/import.html#loading

### 通常 のimporter

PathFinderが使われている模様。

```
>> BuiltinImporter find_spec ('re', None, None) {}
<< BuiltinImporter None
>> FrozenImporter find_spec ('re', None, None) {}
<< FrozenImporter None
>> PathFinder find_spec ('re', None, None) {}
<< PathFinder ModuleSpec(name='re', loader=<_frozen_importlib_external.SourceF
```

どうも実装自体は `importlib._bootstrap_external:PathFinder`にある。
ModuleSpecを返すみたいだけれど。

内部的にはSourceFileLoader

### 自作するときの注意点

https://docs.python.org/ja/3/reference/import.html#loaders

loader.exec_module()

- python moduleの場合には `module.__dict__` でexecするべき
- 上手く動かない例外はぜんぶImportErrorにするべき

loader.create_module()

- Noneを返せば良い感じにmoduleを作ってくれる
- (内部的には [types.ModuleType](https://docs.python.org/ja/3/library/types.html#types.ModuleType))

importer = loader + finder

- importerとして扱うのならfind_spec()で返すloaderにselfを渡せば良い

### sys.path_hooks と sys.meta_path

以下のそれぞれがあるとして、設定場所の話。

- [finder](https://docs.python.org/ja/3/glossary.html#term-finder)
- [loader](https://docs.python.org/ja/3/glossary.html#term-loader)
- [impoter](https://docs.python.org/ja/3/glossary.html#term-importer)

finderには２種類ある

- [path entry finder](https://docs.python.org/ja/3/glossary.html#term-path-entry-finder) -- sys.path_hooks を見る
- [meta path finder](https://docs.python.org/ja/3/glossary.html#term-meta-path-finder) -- sys.meta_path を見る

しかし、 importlib.abc:MetaPathFinder も importlib.abc:PathEntryFinderも古い感じの形式のものだ。

いままででずっと話されていたものは MetaPathFinderの方。

一方でPathEntryFinderの方はMetaPathFinderであるbootstrap.machinary:PathFinder越しに使われるもの (正確に言うと PathFinder._path_hooks()で使われる)。

sys.pathに無いものが渡された場合にsys.path_hooksに登録したものだと困るっぽい。

以下の様なコードが必要になる。

```python
def make_finder(path):
    print("@@", path)
    return DummyImporter


sys.path.append(".")
sys.path_hooks.insert(0, make_finder)
```

必要な理由が分かった。

```python
class FileFinder:

    """File-based finder.

    Interactions with the file system are cached for performance, being
    refreshed when the directory the finder is handling has been modified.

    """

# ...

    def find_spec(self, fullname, target=None):
        """Try to find a spec for the specified module.

        Returns the matching spec, or None if not found.
        """

# ...

        # Check for a file w/ a proper suffix exists.
        for suffix, loader_class in self._loaders:
            full_path = _path_join(self.path, tail_module + suffix)
            _bootstrap._verbose_message('trying {}', full_path, verbosity=2)
            if cache_module + suffix in cache:
                if _path_isfile(full_path):
                    return self._get_spec(loader_class, fullname, full_path,
                                          None, target)


def _path_isfile(path):
    """Replacement for os.path.isfile."""
    return _path_is_mode_type(path, 0o100000)


def _path_is_mode_type(path, mode):
    """Test whether the path is the specified mode type."""
    try:
        stat_info = _path_stat(path)
    except OSError:
        return False
    return (stat_info.st_mode & 0o170000) == mode
```

ここでファイルの存在を確認していた。

### FileFinder

PathFinderからFileFinder経由でいろいろなimporterが実行されているっぽい？

```python

def _install(_bootstrap_module):
    """Install the path-based import components."""
    _setup(_bootstrap_module)
    supported_loaders = _get_supported_file_loaders()
    sys.path_hooks.extend([FileFinder.path_hook(*supported_loaders)])
    sys.meta_path.append(PathFinder)


def _get_supported_file_loaders():
    """Returns a list of file-based module loaders.

    Each item is a tuple (loader, suffixes).
    """
    extensions = ExtensionFileLoader, _imp.extension_suffixes()
    source = SourceFileLoader, SOURCE_SUFFIXES
    bytecode = SourcelessFileLoader, BYTECODE_SUFFIXES
    return [extensions, source, bytecode]
```

## python metaprogrammingを除外した黒魔術って何だろう？

```
黒魔術  = 白魔術の補集合
黒魔術' = 黒魔術 - metaprogramming
metaprogramming = {metaclass, decorator, descriptor, class階層, monkey patch, mocking}
```

debugに役立つという視点は良いかもしれない(つまり製品コードには含まれていない)。

このあたり参考になるかも？

- https://tell-k.github.io/pyconjp2016/#1

ast変換とsys.meta_pathは除外されるべき？

例えばtracebackをイジる系はやっぱり黒魔術'に含まれて良さそう。

簡単なものも混ぜておきたいきがする。

- reifyとかは順序を巧妙に使っているので良い感じ
- signalのhandlingを使ったtracebackは良さそう
- tracebackを使ったnestedな表現の可視化もありなきがする？
- path finderでのhookとかも良いかも(最新のもの)。
- module importでのhookをわざと失敗させて見つける依存を可視化する方法
- rustでやるやつなんだっけ？あれはどうだろう？
- power assertとかは手軽かも？
- egg周りの何かも良いかもしれない？

やばめのもの

- cython的な何かの実装ってふくめるべき？
- forbiddenfruitとか
- macropy的なものとか

hmm

- namedtupleは正直別にすごくはなさそう
- 型をまっとうに使っているのは除外されそう


## dictknife query join

- joinを実装する
- 素直に2要素のtupleにする `x -> y -> (x, y)`
- outer joinを含めると木になる e.g. `x -> y -> (x, Some y)`
- joinをN(N>2)回行う
- `(1, None)`, `(1, (2, None))`, `(1, (2, 3))` が全て発生しうる
- Noneという情報だけだとツライ。

以下の様に変換されて欲しい

```
(1, None) -> (1, (None, None)) -> (1, None, None)
(1, (2, None)) -> (1, 2, None)
(1, (2, 3)) -> (1, 2, 3)
```

つまり Noneのときのzero valueを定義したい。

