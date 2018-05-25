## python struct log

全体が把握できない

使い方

```
rom structlog import get_logger
logger = get_logger()
```

:warning: `structlog.configure()` が必要。これは先頭でやらないとダメそう？

### configureの順序の問題再現した

[./example_structlog/03jsonlog.py](./example_structlog/03jsonlog.py)

これはprocessorsを変えたときの話？

#### configureの仕組み

configureを呼ぶときには

```python
def configure(...):
    # ...
    if processors is not None:
        _CONFIG.default_processors = processors
```


ここで `_CONFIG` は

```python
# _config.py
CONFIG = _Configuration()
```

なのでこの configuration objectが共有されていれば良いのだけれど。

### get logger

get loggerの挙動は以下

```python
wrap_logger(None, logger_factory_args=args, **initial_values)
```

wrap loggerは

```python

def wrap_logger(logger, processors=None, wrapper_class=None,
                context_class=None, cache_logger_on_first_use=None,
                logger_factory_args=None, **initial_values):
    return BoundLoggerLazyProxy(
        logger,
        wrapper_class=wrapper_class,
        processors=processors,
        context_class=context_class,
        cache_logger_on_first_use=cache_logger_on_first_use,
        initial_values=initial_values,
        logger_factory_args=logger_factory_args,
    )
```

processorsを渡していないときにはNone。そしてLazyProxyが作られるのだけれど。bindした時点でlazyじゃなくなる。


```
class BoundLoggerLazyProxy(object):
    # ...
    def bind(self, **new_values):
        _logger = self._logger
        if not _logger:
            _logger = _CONFIG.logger_factory(*self._logger_factory_args)

        if self._processors is None:
            procs = _CONFIG.default_processors
        else:
            procs = self._processors
        # ....
```

原因がわかった。自作の`get_logger()`が以下。

```python
def get_logger(name, *args, **kwargs):
    logger = structlog.get_logger(name).bind(source=name)
    return logger
```

そしてここで無視されるんだ。

```python
class PrintLoggerFactory(object):
    # ...
    def __call__(self, *args):
        # argsは捨てられる
        return PrintLogger(self._file)
```
