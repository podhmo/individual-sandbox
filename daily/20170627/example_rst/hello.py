  # -*- coding:utf-8 -*-
  import logging
  logger = logging.getLogger(__name__)


  def hello():
      logger.info("hello is called")
      return "hello"


  logging.basicConfig(level=logging.INFO)
  logger.info("output:%s", hello())
