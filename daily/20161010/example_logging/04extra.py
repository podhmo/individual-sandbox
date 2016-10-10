import logging
import random
logger = logging.getLogger(__name__)

fmt = "%(message)10s [who=%(who)s cost=%(cost).5f]"
logging.basicConfig(level=logging.DEBUG, format=fmt)

logger.info("hello", extra={"who": "foo", "cost": random.random() * 5})
logger.info("bye", extra={"who": "foo", "cost": random.random() * 5})

logger.info("noop")
