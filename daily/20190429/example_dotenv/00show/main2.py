import os
from dotenv import load_dotenv

load_dotenv(verbose=True, override=True)
print("prefix", os.environ.get("prefix"))
print("message", os.environ.get("message"))
