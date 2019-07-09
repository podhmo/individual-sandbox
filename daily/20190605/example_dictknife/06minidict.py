from describe import describe_dict, p
from dictknife import loading


p(describe_dict(loading.loadfile("people.json")))
p(describe_dict(loading.loadfile("people.json"), life=1))
