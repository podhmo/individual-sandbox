from dictknife import loading
from dictknife.jsonknife import get_resolver
from dictknife.jsonknife import Bundler
import logging
logging.basicConfig(level=logging.DEBUG)
filename = "./src/main.json"
r = get_resolver(filename)
doc = {
    "definitions": {
        "person": r.access_by_json_pointer("/definitions/person"),
    }
}
loading.dumpfile(doc)
b = Bundler(r)
d = b.bundle(doc)
loading.dumpfile(d)
