# from  https://docs.aws.amazon.com/code-samples/latest/catalog/python-sqs-long_polling_receive_message.py.html

import boto3
from handofcats import as_command
from dictknife import loading


@as_command
def run(*, config_path: str) -> None:
    # Create SQS client
    c = loading.loadfile(config_path)
    sqs = boto3.client("sqs")
    queue_url = c["QueueUrl"]

    # Long poll for message on provided SQS queue
    response = sqs.receive_message(
        QueueUrl=queue_url,
        AttributeNames=["SentTimestamp"],
        MaxNumberOfMessages=10,
        MessageAttributeNames=["All"],
        WaitTimeSeconds=20,
        VisibilityTimeout=1,
    )
    messages = response["Messages"]
    print(len(messages))

    will_be_deleted = []
    for msg in messages:
        print(f"{msg['MessageId']}: {msg['Body']}")
        will_be_deleted.append(
            {"Id": msg["MessageId"], "ReceiptHandle": msg["ReceiptHandle"]}
        )
        # delete
    # import inspect
    # print(inspect.getfullargspec(sqs.delete_message_batch))
    print(sqs.delete_message_batch(QueueUrl=queue_url, Entries=will_be_deleted))
