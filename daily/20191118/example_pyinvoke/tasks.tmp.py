import sub  # noqa F401
from invoke import Task

ts = [x for x in globals().values() if isinstance(x, Task)]
namespace = invoke.Collection()
for t in sorted(ts, key=lambda x: x.body.__code__.co_firstlineno):
    namespace.add_task(t)
namespace.add_collection(sub)

