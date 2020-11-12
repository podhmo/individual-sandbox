## python 1ファイルに収められない？

- 遊びでimporterをいじる
- そもそもzip importerで良くない？ https://docs.python.org/ja/3/library/zipimport.html
- zipappとか https://docs.python.org/3/library/zipapp.html

```
sys.paths.insert(0, "zzz.zip")
```

## python そういえば `__set_name__` を使ってschemaライブラリが作れるよね

実際decriptorの説明にもあるな。

- https://docs.python.org/3/howto/descriptor.html#custom-validators

### dataclasses

そもそもがdataclassesをtypeによってvalidateしたいということ？

- https://stackoverflow.com/questions/50563546/validating-detailed-types-in-python-dataclasses/50622643#50622643
