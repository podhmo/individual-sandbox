## python metashape

## python typing

そういえばannotationsをとりだすのはtypingを使ったほうが良いかも？

- `ob.__anotations__` ではなく
- `typing.get_type_hints()` を使う

### pycomment

こういうエラーが出るのか

```
pycomment --inplace 00*.py
Traceback (most recent call last):
  File "VENV/bin/pycomment", line 11, in <module>
    load_entry_point('pycomment', 'console_scripts', 'pycomment')()
  File "VENV/pycomment/pycomment/__main__.py", line 53, in main
    run(args.sourcefile, out=wf)
  File "VENV/pycomment/pycomment/__main__.py", line 9, in run
    capture_result = capture(code, g=g)
  File "VENV/pycomment/pycomment/capture.py", line 14, in capture
    exec(code, g)
  File "<string>", line 25, in <module>
  File "/usr/lib/python3.7/typing.py", line 967, in get_type_hints
    base_globals = sys.modules[base.__module__].__dict__
KeyError: 'exec'
make: *** [Makefile:3: 00] Error 1
```

あー、exec実行しているせいか。runpyで直せそう。

## pythonで物理的にファイルを指定して実行

現状magicalimportでやっているけれどpycacheが残って邪魔かも？
(逆にcacheが聞いているのだけれど)

runpyを使うというのもあり？

- https://docs.python.org/ja/3/library/runpy.html

あるいはcodeop

- https://docs.python.org/ja/3/library/codeop.html
