import queue
from minitask.q import Q
from minitask.q import PickleFormat
from handofcats import as_command


@as_command
def run():
    q = Q(queue.Queue(), format_protcol=PickleFormat())
    for i in range(5):
        q.put({"message": f"hello {i}"})
    q.put(None)

    while True:
        m, task_done = q.get()
        if m is None:
            task_done()
            break
        print(m)
        task_done()
