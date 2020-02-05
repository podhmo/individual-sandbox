from __future__ import annotations
import typing as t
import typing_extensions as tx
import contextlib
import threading
import time
import dataclasses
from functools import partial
import subprocess

from botocore.exceptions import ClientError
import boto3
from handofcats import as_command

from minitask.q import Message
from minitask.langhelpers import reify
from minitask.worker.types import WorkerCallable
from minitask.worker._subprocess import spawn_worker_process
from minitask.worker._subprocess import wait_processes


@dataclasses.dataclass
class Config:
    QueueUrl: str
    AttributeNames: t.List[str] = dataclasses.field(
        default_factory=lambda: ["SentTimestamp"]
    )
    MaxNumberOfMessages: int = 1
    MessageAttributeNames: t.List[str] = dataclasses.field(
        default_factory=lambda: ["All"]
    )
    WaitTimeSeconds: int = 5  # これを長めにする?
    VisibilityTimeout: int = 1


# TODO: aioboto3?
class SQSQueue:
    def __init__(self, config: Config):
        self.config = config

    @reify
    def sqs_client(self):
        return boto3.client("sqs")

    def put(self, body):
        return self.sqs_client.send_message(QueueUrl=self.queue_url, MessageBody=body)

    def get(self) -> t.Tuple[t.Any, t.Callable[[], None]]:
        response = self.sqs_client.receive_message(**dataclasses.asdict(self.config))

        messages = response.get("Messages")  # xxx
        if messages is None:
            return None, lambda: None

        assert len(messages) == 1
        msg = messages[0]

        def _task_done():
            self.sqs_client.delete_message_batch(
                QueueUrl=self.config.QueueUrl,
                Entries=[
                    {"Id": msg["MessageId"], "ReceiptHandle": msg["ReceiptHandle"]}
                ],
            )

        return Message(body=msg["Body"], metadata=msg), _task_done


class Manager:
    def __init__(self, config: t.Optional[Config]):
        self.config = config or Config()

    def spawn(self, target: WorkerCallable, *, uid: str) -> subprocess.Popen[bytes]:
        p = spawn_worker_process(self, target, uid=uid, config=self.config)
        self.processes.append(p)
        return p

    def __len__(self) -> int:
        return len(self.processes)

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

        internal = SQSQueue(self.config)
        yield Q(internal, adapter=None)

    def wait(self, *, check: bool = True) -> None:
        wait_processes(self.processes)

    @reify
    def processes(self) -> t.List[subprocess.Popen[bytes]]:
        return []


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
    with Manager(Config(QueueUrl=queue_url)) as m:
        m.spawn(use, uid=queue_url)
