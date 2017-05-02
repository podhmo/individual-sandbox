from dictknife import loading, pp
from schema import Top

loading.setup()
data = loading.loadfile("./sample.yaml")
data, err = Top().load(data)
if err:
    pp(err)
else:
    loading.dumpfile(data)
