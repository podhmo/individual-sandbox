running completed, without interception.

```
$ go run 02*
*task prepare(1s)
*task start(2s) -- 2016-08-23 00:41:18.302581786 +0900 JST
*task end -- 2016-08-23 00:41:20.308169751 +0900 JST
----------------------------------------
*task prepare(1s)
*task start(2s) -- 2016-08-23 00:41:21.313099909 +0900 JST
*task end -- 2016-08-23 00:41:23.318244232 +0900 JST
----------------------------------------
ok
```

not handling signal.

```
$ go run 02*
*task prepare(1s)
*task start(2s) -- 2016-08-23 00:41:41.994864648 +0900 JST
^Csignal: interrupt
```

handling signal.
(want to intercept running this program, but also want to waiting that first task is ending completely)


```
$ go run 03*
*task prepare(1s)
*task start(2s) -- 2016-08-23 00:43:51.168281895 +0900 JST
^C*task end -- 2016-08-23 00:43:53.169707314 +0900 JST
----------------------------------------
bye
```
