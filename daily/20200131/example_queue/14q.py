import typing as t
import dataclasses
import queue


@dataclasses.dataclass
class Message:
    body: t.Any
    metadata: t.Dict[str, t.Any] = dataclasses.field(default_factory=dict)


def run_priority():
    class Q:
        def __init__(self, q):
            self.q = q

        def __enter__(self) -> "Q":
            self.put(None, priority=10)  # end
            return self

        def __exit__(self, typ, val, tb):
            pass

        def put(self, val, *, priority=1):
            return self.q.put((priority, val))

        def get(self):
            priority, val = self.q.get()
            return Message(val, metadata={"priority": priority}), self.q.task_done

    with Q(queue.PriorityQueue()) as q:
        for i in range(5):
            q.put(i)

    while True:
        m, done = q.get()
        if m.body is None:
            done()
            break
        print(m)
        done()
    print("ok")


def run_fifo():
    class Q:
        def __init__(self, q):
            self.q = q
            self.manager = None

        def __enter__(self) -> "Q":
            return self

        def __exit__(self, typ, val, tb):
            self.put(None)  # end

        def put(self, val):
            return self.q.put(val)

        def get(self):
            val = self.q.get()
            return Message(val), self.q.task_done

    with Q(queue.Queue()) as q:
        for i in range(5):
            q.put(i)
        q.put(None)  # end

    while True:
        m, done = q.get()
        if m.body is None:
            done()  # hmm
            break
        print(m)
        done()
    print("ok")


def run_pickle():
    import pickle

    class Protocol:
        def encode(self, v):
            return pickle.dumps(v)

        def decode(self, b):
            return pickle.loads(b)

    class Q:
        def __init__(self, q, p=None):
            self.q = q
            self.p = p

        def __enter__(self) -> "Q":
            return self

        def __exit__(self, typ, val, tb):
            self.put(None)  # end

        def put(self, val):
            if self.p is not None:
                val = self.p.encode(val)
            return self.q.put(val)

        def get(self):
            val = self.q.get()
            if self.p is not None:
                val = self.p.decode(val)
            return Message(val), self.q.task_done

    q = Q(queue.Queue(), Protocol())
    with q:
        for i in range(5):
            q.put(i)
        q.put(None)  # end

    while True:
        m, done = q.get()
        if m.body is None:
            done()  # hmm
            break
        print(m)
        done()
    print("ok")


run_priority()
run_fifo()
run_pickle()
