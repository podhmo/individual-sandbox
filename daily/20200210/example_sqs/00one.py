import boto3
import os
from botocore.exceptions import ClientError
from handofcats import as_subcommand


@as_subcommand
def sender(*, queue_url: str, group_id: str) -> None:
    print(os.getpid(), "** sender **")
    sqs_client = boto3.client("sqs")

    entries = []
    for i in range(1, 6):
        entries.append(
            {
                "Id": f"m{i}",
                "MessageBody": f"SQS message #{i}",
                "MessageGroupId": group_id,
            }
        )

    try:
        assert (
            entries
        ), "!! An error occurred (AWS.SimpleQueueService.EmptyBatchRequest) when calling the SendMessageBatch operation: There should be at least one SendMessageBatchRequestEntry in the request."

        response = sqs_client.send_message_batch(QueueUrl=queue_url, Entries=entries)
        print(os.getpid(), "SEND", response)
    except ClientError as e:
        print(os.getpid(), "!!", e)


@as_subcommand
def receiver(*, queue_url: str, group_id: str) -> None:
    print(os.getpid(), "** receiver **")

    life = 3
    sqs_client = boto3.client("sqs")
    for _ in range(life):
        try:
            response = sqs_client.receive_message(
                QueueUrl=queue_url,
                AttributeNames=["SentTimestamp"],
                MaxNumberOfMessages=10,
                MessageAttributeNames=["All"],
                WaitTimeSeconds=5,
                VisibilityTimeout=1,
            )
            print(os.getpid(), "RECEIVE", response)
            print(os.getpid(), "----------------------------------------")
            try:
                messages = response["Messages"]
            except KeyError:
                print(os.getpid(), "EMPTY")
                continue

            print(os.getpid(), len(messages))

            will_be_deleted = []
            for msg in messages:
                print(os.getpid(), f"{msg['MessageId']}: {msg['Body']}")
                will_be_deleted.append(
                    {"Id": msg["MessageId"], "ReceiptHandle": msg["ReceiptHandle"]}
                )

            print(
                os.getpid(),
                "DELETE",
                sqs_client.delete_message_batch(
                    QueueUrl=queue_url, Entries=will_be_deleted
                ),
            )
        except ClientError as e:
            print(os.getpid(), "!!", e)


@as_subcommand
def run(*, queue_url: str):
    import sys
    import subprocess

    ps = []
    ps.append(
        subprocess.Popen(
            [
                sys.executable,
                __file__,
                "sender",
                "--queue-url",
                queue_url,
                "--group-id",
                "foo",
            ]
        )
    )
    ps.append(
        subprocess.Popen(
            [
                sys.executable,
                __file__,
                "receiver",
                "--queue-url",
                queue_url,
                "--group-id",
                "foo",
            ]
        )
    )
    ps.append(
        subprocess.Popen(
            [
                sys.executable,
                __file__,
                "sender",
                "--queue-url",
                queue_url,
                "--group-id",
                "bar",
            ]
        )
    )
    ps.append(
        subprocess.Popen(
            [
                sys.executable,
                __file__,
                "receiver",
                "--queue-url",
                queue_url,
                "--group-id",
                "bar",
            ]
        )
    )
    ps.append(
        subprocess.Popen(
            [
                sys.executable,
                __file__,
                "receiver",
                "--queue-url",
                queue_url,
                "--group-id",
                "bar",
            ]
        )
    )
    for p in ps:
        p.wait()


as_subcommand.run()
