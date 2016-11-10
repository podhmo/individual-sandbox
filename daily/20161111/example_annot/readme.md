tail-callか調べるやつ。

```
$ make build
$ make clean
$ grep -A 1 call f.annot
call(
  stack
$ grep -A 1 call g.annot
call(
  tail
$ grep -A 1 call h.annot
call(
  tail
--
call(
  tail
```
