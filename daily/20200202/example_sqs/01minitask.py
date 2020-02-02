from __future__ import annotations
import contextlib
import typing as t
import typing_extensions as tx
import threading
import time
from functools import partial
from botocore.exceptions import ClientError
import boto3
from handofcats import as_command
from minitask.q import Message
from minitask.langhelpers import reify
from minitask.worker.subprocessworker import Manager as _Manager


# TODO: aioboto3?
class SQSQueue:
    def __init__(self, queue_url: str):
        self.queue_url = queue_url

    @reify
    def sqs_client(self):
        import boto3

        return boto3.client("sqs")

    def put(self, body):
        return self.sqs_client.send_message(QueueUrl=self.queue_url, MessageBody=body)

    def get(self) -> t.Tuple[t.Any, t.Callable[[], None]]:
        # TODO: defaultの設定を変えられるように
        response = self.sqs_client.receive_message(
            QueueUrl=self.queue_url,
            AttributeNames=["SentTimestamp"],
            MaxNumberOfMessages=1,
            MessageAttributeNames=["All"],
            WaitTimeSeconds=5,  # これを長めにする?
            VisibilityTimeout=1,
        )
        messages = response.get("Messages")  # xxx
        if messages is None:
            return None, lambda: None
        assert len(messages) == 1
        msg = messages[0]

        def _task_done():
            self.sqs_client.delete_message_batch(
                QueueUrl=self.queue_url,
                Entries=[
                    {"Id": msg["MessageId"], "ReceiptHandle": msg["ReceiptHandle"]}
                ],
            )

        return Message(body=msg["Body"], metadata=msg), _task_done


import subprocess
from minitask.worker.types import WorkerCallable
from minitask.worker._subprocess import spawn_worker_process
from minitask.worker._subprocess import wait_processes


class Manager:
    class OptionDict(tx.TypedDict):
        queue_url: str

    @classmethod
    def from_dict(cls, kwargs: Manager.OptionDict) -> Manager:

        return cls(**kwargs)

    def __init__(self, queue_url: str):
        self.queue_url = queue_url
        self.processes: t.List[subprocess.Popen[bytes]] = []

    def spawn(self, target: WorkerCallable, *, uid: str) -> subprocess.Popen[bytes]:
        p = spawn_worker_process(
            self, target, uid=uid, option_type=self.__class__.OptionDict
        )
        self.processes.append(p)
        return p

    def __len__(self) -> int:
        return len(self.processes)

    def wait(self, *, check: bool = True) -> None:
        wait_processes(self.processes)

    def __enter__(self) -> Manager:
        return self

    def __exit__(
        self,
        exc: t.Optional[t.Type[BaseException]],
        value: t.Optional[BaseException],
        tb: t.Any,
    ) -> tx.Literal[False]:
        self.wait()

    @contextlib.contextmanager
    def open_reader_queue(self, uid: str):
        from minitask.q import Q

        internal = SQSQueue(self.queue_url)
        yield Q(internal, adapter=None)


def use(m: Manager, uid: str) -> None:
    with m.open_reader_queue(uid) as q:
        for item in q:
            print("<-", item)


@as_command
def run(*, queue_url: str):
    def provider(queue_url: str):
        sqs_client = boto3.client("sqs")
        try:
            for i in range(1, 10):
                m = f"SQS message #{i}"
                sqs_client.send_message(QueueUrl=queue_url, MessageBody=m)
                time.sleep(0.1)
        except ClientError as e:
            print("!!", e)

    th = threading.Thread(target=partial(provider, queue_url))
    th.start()

    with Manager(queue_url) as m:
        m.spawn(use, uid=queue_url)
