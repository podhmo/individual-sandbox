import boto3
import os
from botocore.exceptions import ClientError
from handofcats import as_subcommand


@as_subcommand
def sender(*, queue_url: str, group_id: str = "default") -> None:
    print(os.getpid(), "** sender **")
    # breakpoint()
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


as_subcommand.run()
