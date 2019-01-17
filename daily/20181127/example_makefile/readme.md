```console
$ make
test -n ""
make: *** [default] Error 1

$ make ZZZ=333
test -n "333"
echo ok
ok
```
