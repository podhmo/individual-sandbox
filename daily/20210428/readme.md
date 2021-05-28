## python pretty exception

- pretty-exception
- pretty-error
- tbvaccine

## poetry editable install

poetry add `--editable` が使える様になった

- https://github.com/python-poetry/poetry/pull/3940

しかしこれは依存ライブラリのものっぽい？

### hmm

- https://github.com/python-poetry/poetry/issues/1279
- https://github.com/pypa/pip/issues/6314#issuecomment-469176276

flitは `-s` でいけた気がする。

- https://github.com/pfmoore/editables

### work-around

```
$ tar -tf dist/foo-0.1.0.tar.gz
$ tar -O -xf dist/foo-0.1.0.tar.gz foo-0.1.0/setup.py > setup.py
$ rm pyproject.toml
$ pip install -e .
```

### poetry new

```console
$ poetry new foo
```

### poetry

キレイな例外の出力


