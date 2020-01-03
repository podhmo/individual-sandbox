# from https://docs.aws.amazon.com/code-samples/latest/catalog/python-sqs-receive_message.py.html

import logging
import boto3
from botocore.exceptions import ClientError
from handofcats import as_command
from dictknife import loading

# from https://docs.aws.amazon.com/code-samples/latest/catalog/python-sqs-send_message.py.html


def retrieve_sqs_messages(sqs_queue_url, num_msgs=1, wait_time=0, visibility_time=5):
    """Retrieve messages from an SQS queue

    The retrieved messages are not deleted from the queue.

    :param sqs_queue_url: String URL of existing SQS queue
    :param num_msgs: Number of messages to retrieve (1-10)
    :param wait_time: Number of seconds to wait if no messages in queue
    :param visibility_time: Number of seconds to make retrieved messages
        hidden from subsequent retrieval requests
    :return: List of retrieved messages. If no messages are available, returned
        list is empty. If error, returns None.
    """

    # Validate number of messages to retrieve
    if num_msgs < 1:
        num_msgs = 1
    elif num_msgs > 10:
        num_msgs = 10

    # Retrieve messages from an SQS queue
    sqs_client = boto3.client("sqs")
    try:
        msgs = sqs_client.receive_message(
            QueueUrl=sqs_queue_url,
            MaxNumberOfMessages=num_msgs,
            WaitTimeSeconds=wait_time,
            VisibilityTimeout=visibility_time, # default is 30
        )
    except ClientError as e:
        logging.error(e)
        return None

    # Return the list of retrieved messages
    # {'ResponseMetadata': {'RequestId': 'b334afdb-62d0-52d4-b350-f953474d35da', 'HTTPStatusCode': 200, 'HTTPHeaders': {'x-amzn-requestid': 'b334afdb-62d0-52d4-b350-f953474d35da', 'date': 'Thu, 02 Jan 2020 17:07:41 GMT', 'content-type': 'text/xml', 'content-length': '240'}, 'RetryAttempts': 0}}
    return msgs["Messages"]


def delete_sqs_message(sqs_queue_url, msg_receipt_handle):
    """Delete a message from an SQS queue

    :param sqs_queue_url: String URL of existing SQS queue
    :param msg_receipt_handle: Receipt handle value of retrieved message
    """

    # Delete the message from the SQS queue
    sqs_client = boto3.client("sqs")
    sqs_client.delete_message(QueueUrl=sqs_queue_url, ReceiptHandle=msg_receipt_handle)


@as_command
def run(*, config_path: str):
    """Exercise retrieve_sqs_messages()"""

    # Assign this value before running the program
    c = loading.loadfile(config_path)
    sqs_queue_url = c["QueueUrl"]
    num_messages = 10

    # Set up logging
    logging.basicConfig(
        level=logging.INFO,
        format="%(levelname)s: %(asctime)s: %(message)s"
        # level=logging.DEBUG, format="%(levelname)s: %(asctime)s: %(message)s"
    )

    # Retrieve SQS messages
    msgs = retrieve_sqs_messages(sqs_queue_url, num_messages)
    if msgs is not None:
        for msg in msgs:
            logging.info(
                f'SQS: Message ID: {msg["MessageId"]}, ' f'Contents: {msg["Body"]}'
            )

            # Remove the message from the queue
            delete_sqs_message(sqs_queue_url, msg["ReceiptHandle"])
