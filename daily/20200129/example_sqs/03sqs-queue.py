import typing as t
from functools import cached_property
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
    q = SQSQueue(queue_url=queue_url)
    while True:
        item, task_done = q.get()
        if item is None:
            task_done()
            break
        print("<-", item)
        task_done()
