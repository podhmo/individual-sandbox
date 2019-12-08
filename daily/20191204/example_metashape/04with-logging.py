import logging
import runpy
import sys

logging.basicConfig(level=logging.DEBUG)
runpy.run_path(sys.argv[1], run_name=sys.argv[1])
