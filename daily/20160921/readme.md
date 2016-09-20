# python structlog

structlog

```python
import structlog
import logging

logger = structlog.get_logger()
logger = logger.bind(user="anonymous", some_key=23)

logging.basicConfig(level=logging.DEBUG)
logger.info("user.logged_in")
```
