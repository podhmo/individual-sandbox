## python pycomment

同じ位置のファイルのimportができないっぽい。

```console
$ pycomment 05*.py
  File "VENV/individual-sandbox/daily/20200105/example_metashape/tmpboqym1mj.py", line 56, in <module>
ModuleNotFoundError: No module named 'model'

$ python 05*.py
```

どうもrunpyで実行したときのsys.pathが異なる模様 (afterがrunpy) 。

```diff
--- before	2020-01-05 19:54:10.000000000 +0900
+++ after	2020-01-05 19:53:46.000000000 +0900
@@ -1,4 +1,4 @@
-'VENV/individual-sandbox/daily/20200105/example_metashape'
+'VENV/bin'
 '/opt/local/Library/Frameworks/Python.framework/Versions/3.8/lib/python38.zip'
 '/opt/local/Library/Frameworks/Python.framework/Versions/3.8/lib/python3.8'
 '/opt/local/Library/Frameworks/Python.framework/Versions/3.8/lib/python3.8/lib-dynload'
```

## python deferの代わり

contextlib.ExitStackのcallbackってdeferの代わりになったっけ？なりそう。
