from __future__ import annotations
import time
from botocore.exceptions import ClientError
from handofcats import as_command
from minitask.worker.sqsworker import Manager, Config


def receiver(m: Manager, uid: str) -> None:
    with m.open_reader_queue(uid) as q:
        for item in q:
            # print("@", q.latest)
            print("<-", item)


def sender(m: Manager, uid: str) -> None:
    with m.open_writer_queue(uid) as q:
        try:
            for i in range(1, 10):
                m = f"SQS message #{i}"
                q.put({"message": m})
                time.sleep(0.1)
        except ClientError as e:
            print("!!", e)


@as_command
def run(*, queue_url: str):
    with Manager(Config(MessageAttributeNames=["All"])) as m:
        m.spawn(sender, uid=queue_url)
        m.spawn(receiver, uid=queue_url)
