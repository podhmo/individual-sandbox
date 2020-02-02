import queue
from minitask.q import Q
from minitask.q import PickleFormat
from minitask.q import consume
from handofcats import as_command


@as_command
def run():
    q = Q(queue.Queue(), format_protcol=PickleFormat())
    for i in range(5):
        q.put(i)
    q.put(None)

    for i in consume(q):
        print(i)
