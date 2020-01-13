## python handofcats

- multi commandに対応できた
- 意外とコードの削除がめんどくさい
- pipxとか使えばzero dependenciesもイケるかも？
- 次は3.0.0にしようかな
- updateが必要なのは？

  - prestring
  - handofcats

## argparseの小ネタ

- subcommandの部分の表示を考えるとtitleをつけたほうが良い
- FormatterClass
- @マーク付きで取り出せる

## make この形が多分一番良い感じ

```make
00:
	python $(shell echo $@*.py) | tee $(patsubst %.py,%.output,$(shell echo $@*.py))
01:
	python $(shell echo $@*.py) | tee $(patsubst %.py,%.output,$(shell echo $@*.py))


clean:
	rm -f *.output
.PHONY: clean
```

## argparse FormatterClass

- https://docs.python.org/3/library/argparse.html#formatter-class

- class argparse.RawDescriptionHelpFormatter¶
- class argparse.RawTextHelpFormatter
- class argparse.ArgumentDefaultsHelpFormatter
- class argparse.MetavarTypeHelpFormatter

### metavar

formatter_class=argparse.MetavarTypeHelpFormatter,

- messageのmetavarが空だとだめなのか。。

```
  File "/opt/local/Library/Frameworks/Python.framework/Versions/3.8/lib/python3.8/argparse.py", line 710, in _get_default_metavar_for_positional
    return action.type.__name__
AttributeError: 'NoneType' object has no attribute '__name__'
```

### argparseって@付きでファイルの中身を渡すみたいなこともできたんだ

https://docs.python.org/3/library/argparse.html#fromfile-prefix-chars

あんまり使えなかった。改行区切りっぽい。

- Replace A with B.
