import boto3
from botocore.exceptions import ClientError
from dictknife import loading
from handofcats import as_command


@as_command
def run(*, queue_url: str):
    """Exercise send_sqs_message()"""

    # Assign this value before running the program
    sqs_client = boto3.client("sqs")
    try:
        response = sqs_client.receive_message(
            QueueUrl=queue_url,
            AttributeNames=["SentTimestamp"],
            MaxNumberOfMessages=10,
            MessageAttributeNames=["All"],
            WaitTimeSeconds=2,
            VisibilityTimeout=1,
        )
        print(loading.dumpfile(response))
        print("----------------------------------------")
        try:
            messages = response["Messages"]
        except KeyError:
            print("EMPTY")
            return

        print(len(messages))

        will_be_deleted = []
        for msg in messages:
            print(f"{msg['MessageId']}: {msg['Body']}")
            will_be_deleted.append(
                {"Id": msg["MessageId"], "ReceiptHandle": msg["ReceiptHandle"]}
            )

        print(
            sqs_client.delete_message_batch(QueueUrl=queue_url, Entries=will_be_deleted)
        )
    except ClientError as e:
        print("!!", e)
