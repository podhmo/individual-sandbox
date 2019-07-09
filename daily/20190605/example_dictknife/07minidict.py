from describe import describe_dict, p
from dictknife import loading

name = "../../20190218/example_openapi/src/petstore.3.0.json"
p(describe_dict(loading.loadfile(name)))
