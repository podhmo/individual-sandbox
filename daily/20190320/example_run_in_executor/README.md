build

```console
$ pip install -r requirements.txt
$ make build
```

check

```console
$ make build
$ make check
PYTHONPATH=. python -c 'import mysleep; print(mysleep.mysleep("OK", 1))'
('    ... start mysleep', 'OK', 1)
('    ... end mysleep', 'OK')
OK
```

## run_in_executor()を使ってもブロックしてしまって上手く処理を逃がせないと感じたとき

awaitableな関数群で構成されていないコードセットを無理やり非同期化してお茶を濁す時にrun_in_executor()が使われる

thread pool executorを使った方。4s掛かっている(1+1+2)

```console
$ PYTHONPATH=. python 00use_thread_pool_executor.py
2019-03-20 20:54:59,837INFO:__main__:**start**
('    ... start mysleep', 'x', 1)
('    ... end mysleep', 'x')
('    ... start mysleep', 'y', 1)
('    ... end mysleep', 'y')
('    ... start mysleep', 'z', 2)
('    ... end mysleep', 'z')
2019-03-20 20:55:03,846WARNING:asyncio:Executing <Task pending coro=<run() running at 00use_thread_pool_executor.py:18> wait_for=<_GatheringFuture pending cb=[<TaskWakeupMethWrapper object at 0x7f4d1a05a828>()] created at /usr/lib/python3.7/asyncio/tasks.py:615> cb=[_run_until_complete_cb() at /usr/lib/python3.7/asyncio/base_events.py:158] created at /usr/lib/python3.7/asyncio/base_events.py:563> took 4.009 seconds
2019-03-20 20:55:03,850INFO:__main__:**end** ['x', 'y', 'z'] 4.012997150421143
```

process pool executorを使っている方。2sで済む(max(1,1,2))

```console
PYTHONPATH=. python 01use_process_pool_executor.py
2019-03-20 20:55:03,959INFO:__main__:**start**
('    ... start mysleep', 'x', 1)
('    ... start mysleep', 'y', 1)
('    ... start mysleep', 'z', 2)
('    ... end mysleep', 'x')
('    ... end mysleep', 'y')
2019-03-20 20:55:04,967INFO:asyncio:poll took 1001.933 ms: 1 events
('    ... end mysleep', 'z')
2019-03-20 20:55:05,969INFO:__main__:**end** ['x', 'y', 'z'] 2.0095720291137695
```

## gilを開放すると

mysleep.pyxを変えて[GILを開放する](https://cython.readthedocs.io/en/latest/src/userguide/external_C_code.html#releasing-the-gil)とThreadPoolExecutorでも2s(max(1,1,2))で終わる様になる。

例えば以下の様な変更を加える。

```diff
diff --git a/daily/20190320/example_run_in_executor/mysleep.pyx b/daily/20190320/example_run_in_executor/mysleep.pyx
index c27e4da..bbf6764 100644
--- a/daily/20190320/example_run_in_executor/mysleep.pyx
+++ b/daily/20190320/example_run_in_executor/mysleep.pyx
@@ -2,6 +2,7 @@ from posix.unistd cimport sleep
 
 def mysleep(x, unsigned int n):
     print("    ... start mysleep", x, n)
-    sleep(n)
+    with nogil:
+        sleep(n)
     print("    ... end mysleep", x)
     return x
```

2sで終わる

```console
$ PYTHONPATH=. python 00use_thread_pool_executor.py
2019-03-21 15:12:40,999INFO:__main__:**start**
('    ... start mysleep', 'x', 1)
('    ... start mysleep', 'y', 1)
('    ... start mysleep', 'z', 2)
('    ... end mysleep', 'x')
('    ... end mysleep', 'y')
2019-03-21 15:12:42,004INFO:asyncio:poll took 1000.853 ms: 1 events
('    ... end mysleep', 'z')
2019-03-21 15:12:43,005INFO:__main__:**end** ['x', 'y', 'z'] 2.00624680519104
```
