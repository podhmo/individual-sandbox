import typing as t
import threading
import time
from functools import cached_property
from functools import partial
from botocore.exceptions import ClientError
import boto3
from handofcats import as_command


# TODO: aioboto3?
class SQSQueue:
    def __init__(self, *, queue_url: str):
        self.queue_url = queue_url

    @cached_property
    def sqs_client(self):
        import boto3

        return boto3.client("sqs")

    def put(self, body):
        return self.sqs_client.send_message(QueueUrl=self.queue_url, MessageBody=body)

    def get(self) -> t.Tuple[t.Any, t.Callable[[], None]]:
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

        return msg["Body"], _task_done


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

    q = SQSQueue(queue_url=queue_url)
    while True:
        item, task_done = q.get()
        if item is None:
            task_done()
            break
        print("<-", item)
        task_done()
