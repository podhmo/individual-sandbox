## ほしい

links的なcli?command。
your own custom repository的な。

雑に言うと、cookiecutterのrepositoryを覚えるのが面倒。
変にdbでも管理したくない。githubをURL代わりにできない？
可能なら誰かの設定をfollowできる。

- sync


## pypi custom pypiserver

- https://devpi.net/docs/devpi/devpi/latest/+doc/index.html
- https://github.com/pypiserver/pypiserver

### pypiserver

- https://javawithravi.com/setting-up-a-custom-pypi-server/
- https://javawithravi.com/pip-custom-pypi-server/

### 実際に使う

- https://packaging.python.org/guides/using-testpypi/

```console
$ pip install --index-url https://test.pypi.org/simple/ --extra-index-url https://pypi.org/simple your-package
```

.pypirc
```
[testpypi]
username = <your TestPyPI username>
```
