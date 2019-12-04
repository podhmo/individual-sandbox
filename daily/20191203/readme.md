## metashape? prestring?

良い感じにimportしていきたい感じ。
とはいえコード上でソレそのものを記述するのではない感じ。
strすると良い感じにimportされて、callすると自分自身が記述される。


## morphdom

- https://github.com/patrick-steele-idem/morphdom

なんかいつの間にか内部でvdomつかうようになっているのか？

### omi

何かomiやomimと言うやつを発見した。

https://github.com/Tencent/omi

## mkdocs mkdocs-material触ってみた

newしてserve剃る感じ。

## diff highlightと似たような出力を手にしたかった

```python
print("\x1b[31m-\x1b[7mhello\x1b[0m\x1b[31m world\x1b[0m")
print("\x1b[32m+\x1b[7mbye-bye\x1b[0m\x1b[32m world\x1b[0m")
```

## git diff highlight

macでは持ってこないとダメ。

```console
$ sudo ln -s /usr/local/share/git-core/contrib/diff-highlight/diff-highlight /usr/local/bin/
```

.gitconfig

```
[pager]
    log = diff-highlight | less
    show = diff-highlight | less
    diff = diff-highlight | less
```

see

- https://udomomo.hatenablog.com/entry/2019/12/01/181404
