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
2019-03-20 20:55:05,969INFO:__main__:**end** ['x', 'y', 'z'] 2.0095720291137695```

## gilを開放すると

ThreadPoolExecutorでも2s(max(1,1,2))で終わる様になる。

```

```
