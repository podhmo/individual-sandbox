## python jinja2

- filepathの計算方法を変える
- includeで失敗したときのwhereはtemplate path
- includeで404のときにもtemplateの位置を表示したい
- include内で1/0したときは？


### include内で1/0したとき

通常の例外になる？Environment.handle_exceptionが鍵？

もう少し除いたらdebugのところで変更している諸々が便利。

### 条件

- toplevelでruntime error
- toplevelでmacroの利用に失敗
- toplevelでfilterを使いfilterの内部で失敗

最大

```
python
jinja2
python
```

通常

```
python
jinja2
```
