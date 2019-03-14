import logging
import os

# ログ設定(本来はここで直接basicConfigを呼ぶことは少ないけれど)
LOGGER = logging.getLogger("app")
logging.basicConfig(level=getattr(logging, os.environ.get("LOGLEVEL") or "INFO"))
