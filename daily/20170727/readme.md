## python line_profiler

line profilerの使い方毎回忘れてしまう。

```bash
$ pip install line_profiler
```

target.py

```
@profile
def target_function():
    # do something
```

```
$ kernprof -l target.py
$ python -m line_profiler target.py.lprof
```
