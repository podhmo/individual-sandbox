import typing as t
import dataclasses
import queue


@dataclasses.dataclass
class Message:
    body: t.Any
    metadata: t.Dict[str, t.Any] = dataclasses.field(default_factory=dict)


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

    p = Protocol()
    q = queue.PriorityQueue()

    q.put(p.encode(p.create_message(None, priority=10)))  # end
    for i in range(5):
        q.put(p.encode(p.create_message(i)))

    while True:
        message = p.decode(q.get())
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

    p = Protocol()
    q = queue.Queue()

    for i in range(5):
        q.put(p.encode(p.create_message(i)))
    q.put(p.encode(p.create_message(None)))  # end

    while True:
        message = p.decode(q.get())
        if message.body is None:
            q.task_done()  # hmm
            break
        print(message)
        q.task_done()
    print("ok")


run_priority()
run_fifo()
