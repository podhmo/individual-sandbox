```
$ python 00test.py
INFO:client:->: pow args=(2, 3) kwargs={}
127.0.0.1 - - [14/Oct/2016 23:04:19] "POST /RPC2 HTTP/1.1" 200 -
INFO:client:<- 8
INFO:client:->: pow args=(2, -3) kwargs={}
127.0.0.1 - - [14/Oct/2016 23:04:19] "POST /RPC2 HTTP/1.1" 200 -
INFO:client:<- 0.125
INFO:client:->: atan args=(1,) kwargs={}
127.0.0.1 - - [14/Oct/2016 23:04:19] "POST /RPC2 HTTP/1.1" 200 -
INFO:client:<- 0.7853981633974483
.INFO:client:->: pow args=(10, 3) kwargs={}
127.0.0.1 - - [14/Oct/2016 23:04:19] "POST /RPC2 HTTP/1.1" 200 -
INFO:client:<- 1000
INFO:client:->: pow args=(10, -3) kwargs={}
127.0.0.1 - - [14/Oct/2016 23:04:19] "POST /RPC2 HTTP/1.1" 200 -
INFO:client:<- 0.001
.
----------------------------------------------------------------------
Ran 2 tests in 0.201s

OK
```

ファイル名に00を付けてしまったばっかりに直接importできないよ。。
