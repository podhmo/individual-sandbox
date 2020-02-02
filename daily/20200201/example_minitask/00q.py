import queue
from minitask.q import Q
from handofcats import as_command

@as_command
def run():
    q = Q(queue.Queue())
    for i in range(5):
        q.put(i)
    q.put(None)

    while True:
        m, task_done = q.get()
        if m is None:
            task_done()
            break
        print(m)
        task_done()
