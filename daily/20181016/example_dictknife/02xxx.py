from dictknife.transform import flatten
from dictknife import loading

d = loading.loadfile("00person.json")
loading.dumpfile(d, format="json")
loading.dumpfile(flatten(d), format="json")

print("----------------------------------------")

d = loading.loadfile("02person.json")
loading.dumpfile(d, format="json")
loading.dumpfile(flatten(d), format="json")
