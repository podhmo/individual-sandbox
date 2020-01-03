import boto3
from botocore.exceptions import ClientError

from handofcats import as_command
from dictknife import loading


@as_command
def run(*, config_path: str):
    """Exercise send_sqs_message()"""

    # Assign this value before running the program
    c = loading.loadfile(config_path)
    sqs_queue_url = c["QueueUrl"]
    sqs_client = boto3.client("sqs")

    # Send some SQS messages
    entries = []
    for i in range(1, 6):
        entries.append(
            {"Id": f"m{i}", "MessageBody": f"SQS message #{i}", "DelaySeconds": 10}
        )

    try:
        assert (
            entries
        ), "!! An error occurred (AWS.SimpleQueueService.EmptyBatchRequest) when calling the SendMessageBatch operation: There should be at least one SendMessageBatchRequestEntry in the request."

        response = sqs_client.send_message_batch(
            QueueUrl=sqs_queue_url, Entries=entries
        )
        print(response)
    except ClientError as e:
        print("!!", e)
