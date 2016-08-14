# `python setup.py test` のこと

[コチラの記事](http://qiita.com/Kensuke-Mitsuzawa/items/7717f823df5a30c27077)に概ね同意なのだけれど。
test_all作る必要ないんじゃないかなと思ったりした。

こんな感じで良さそうだし。

```
$ cd foo
$ python setup.py test
running test
running egg_info
writing top-level names to foo.egg-info/top_level.txt
writing dependency_links to foo.egg-info/dependency_links.txt
writing foo.egg-info/PKG-INFO
reading manifest file 'foo.egg-info/SOURCES.txt'
writing manifest file 'foo.egg-info/SOURCES.txt'
running build_ext
test_it (foo.tests.test_byebye.Tests) ... ok
test_it (foo.tests.test_hello.Tests) ... ok

----------------------------------------------------------------------
Ran 2 tests in 0.001s

OK
```
