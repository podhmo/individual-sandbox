import os
import logging
from dictknife import loading
from dictknife.jsonknife.resolver import ExternalFileResolver
from dictknife.jsonknife import Bundler

logging.basicConfig(level=logging.DEBUG)
filename = "./src/main.json"
r = ExternalFileResolver(
    f"{os.path.dirname(__file__)}/xxx.json",
    loader=loading,
    doc={
        "definitions": {
            "person": {
                "$ref": f"{filename}#/definitions/person"
            },
        },
    },
)
b = Bundler(r)
d = b.bundle()
loading.dumpfile(d)
