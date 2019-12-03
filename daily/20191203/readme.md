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
