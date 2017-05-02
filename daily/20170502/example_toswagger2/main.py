from dictknife import loading, pp
from schema import Conf

loading.setup()
data = loading.loadfile("./config-sample.json")
data, err = Conf().load(data)
if err:
    pp(err)
else:
    loading.dumpfile(data)
