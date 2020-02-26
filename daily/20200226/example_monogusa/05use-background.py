import typing as t
import os
from monogusa import events
from monogusa import default_once
from minitask.q import Q
from minitask.worker.sqsworker import Manager


@events.subscribe(events.MessageEvent[t.Any])
def on_message(ev: events.MessageEvent[t.Any], q: Q) -> None:
    print(os.getpid(), "!")
    q.put(ev.content)


@default_once
def queue() -> Q[str]:
    import atexit

    m = Manager()
    m = m.__enter__()

    def _teardown():
        nonlocal m
        m.__exit__(None, None, None)  # todo: cache exception

    atexit.register(_teardown)

    # todo: skip
    uid = m.generate_uid(
        "https://ap-northeast-1.queue.amazonaws.com/784330574880/minitask_foo.fifo"
    )
    m.spawn(consume, uid=uid)
    q = m.open_writer_queue(uid)

    def _teardown2():
        q.__exit__(None, None, None)  # todo: cache exception

    atexit.register(_teardown2)
    return q.__enter__()


def consume(m: Manager, uid: str):
    with m.open_reader_queue(uid) as q:
        for item in q:
            print(os.getpid(), "<-", item)


def sample() -> None:
    """fake event stream"""
    from monogusa.cli.events import console_stream, run_stream

    examples = """\
Message, hello
Message, byebye
"""
    run_stream(console_stream(default=examples, sep=","))


if __name__ == "__main__":
    from monogusa.cli import run

    run()
