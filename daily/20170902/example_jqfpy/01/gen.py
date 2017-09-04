import uuid
import sys
import dictknife.loading as loading


def randname():
    return uuid.uuid4().hex[:6]


for i in range(5):
    d = {"name": randname(), "i": i}
    loading.dumpfile(d, format="json")
    sys.stdout.flush()
