import typing as t
import dataclasses
import queue


@dataclasses.dataclass
class Message:
    body: t.Any
    metadata: t.Dict[str, t.Any] = dataclasses.field(default_factory=dict)


class Q:
    def __init__(self, q, p):
        self.q = q
        self.p = p

    def put(self, val, **metadata):
        m = self.p.create_message(val, **metadata)
        return self.q.put(self.p.encode(m))

    def get(self):
        m = self.p.decode(self.q.get())
        return m

    def task_done(self):
        self.q.task_done()  # xxx:


def run_priority():
    class Protocol:
        def create_message(self, v, **kwargs):
            metadata = {"priority": 1}
            metadata.update(kwargs)
            return Message(body=v, metadata=metadata)

        def encode(self, message):
            return message.metadata["priority"], message.body

        def decode(self, b):
            priority, v = b
            return Message(v, metadata={"priority": priority})

    q = Q(queue.PriorityQueue(), Protocol())
    q.put(None, priority=10)  # end
    for i in range(5):
        q.put(i)

    while True:
        message = q.get()
        if message.body is None:
            q.task_done()  # hmm
            break
        print(message)
        q.task_done()
    print("ok")


def run_fifo():
    class Protocol:
        def create_message(self, v, **kwargs):
            return Message(body=v, metadata=kwargs)

        def encode(self, message):
            return message.body

        def decode(self, b):
            return Message(body=b)

    q = Q(queue.Queue(), Protocol())
    for i in range(5):
        q.put(i)
    q.put(None)  # end

    while True:
        message = q.get()
        if message.body is None:
            q.task_done()  # hmm
            break
        print(message)
        q.task_done()
    print("ok")


run_priority()
run_fifo()
