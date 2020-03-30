## git patchを適用

patchの作成

```console
$ git format-patch <revision> -o <dir name>
```

patchの適用

```console
$ git am --3way <dir name>/*.patch
```

## git 特定のdiffをとってlintを実行したい

```console
$ git diff remotes/origin/develop --name-status | grep -P "^(M|A)" | grep -v vendor | grep -v /gen/ | ruby -ne 'puts $2 if $_ =~ %r@(^A|M)\s+(.+)/([^/]+).go$@' | sort | uniq
```
