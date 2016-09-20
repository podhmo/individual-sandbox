# -*- coding:utf-8 -*-
from structlog import get_logger
import logging

logging.basicConfig(level=logging.DEBUG)

logger = get_logger()
logger = logger.bind(user="anonymous", some_key=23)

logger.info('user.logged_in', happy=True)
