# python pymongo

pymongoのsampleとか作っておきたい。

# wip python structlog

structlog

```python
import structlog
import logging

logger = structlog.get_logger()
logger = logger.bind(user="anonymous", some_key=23)

logging.basicConfig(level=logging.DEBUG)
logger.info("user.logged_in")
```

# python tz付きのdatetime object

```python
import pytz
jst = pytz.timezone('Japan')
print(datetime.now().replace(tzinfo=jst).strftime("%Y-%m-%dT%H:%M:%S.%f%z"))
```
