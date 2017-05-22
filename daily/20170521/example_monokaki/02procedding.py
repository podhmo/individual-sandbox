import logging
from monokaki import StructuralFormatter
from monokaki import get_logger
import sys

logger = get_logger("my log")
handler = logging.StreamHandler(sys.stdout)
handler.setFormatter(StructuralFormatter())
logger.logger.addHandler(handler)
logger.setLevel(logging.INFO)
logger.info("hai", name="foo", age=20)
logger.logger.info("hai name=%s, age=%d", "foo", 20)
