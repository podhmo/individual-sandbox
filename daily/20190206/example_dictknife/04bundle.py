from dictknife import loading
from dictknife.jsonknife import bundle
import logging
logging.basicConfig(level=logging.DEBUG)

filename = "./src/main.json#/definitions/person"
d = bundle(filename)
loading.dumpfile(d)
