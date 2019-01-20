import os
import logging
logger = logging.getLogger(__name__)

logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL", "INFO")))
f = open(__file__)
print(len(f.read().split("\n")))
logger.warn("xxxx")
