import dataclasses
import logging
from dictknife import loading
from magicalimport import import_symbol


logging.basicConfig(level=logging.DEBUG)
target = import_symbol("./conf.py:toplevel", here=__file__)
d = dataclasses.asdict(target)
loading.dumpfile(d, format="yaml")
