from dictknife import loading
from dictknife.jsonknife import get_resolver
from dictknife.jsonknife import Bundler

filename = "./src/main.json"
r = get_resolver(filename)
r.doc = {
    "definitions": {
        "person": r.access_by_json_pointer("/definitions/person"),
    }
}
b = Bundler(r)
d = b.bundle()
loading.dumpfile(d)
