from describe import describe_dict, p
from dictknife import loading

p(describe_dict(loading.loadfile("person.json")))
p(describe_dict(loading.loadfile("person.json"), life=1))
p(describe_dict(loading.loadfile("person.json"), life=2))
