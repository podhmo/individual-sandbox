import logging
import boto3
from botocore.exceptions import ClientError

from handofcats import as_command
from dictknife import loading

# from https://docs.aws.amazon.com/code-samples/latest/catalog/python-sqs-send_message.py.html


def send_sqs_message(sqs_queue_url, msg_body):
    """

    :param sqs_queue_url: String URL of existing SQS queue
    :param msg_body: String message body
    :return: Dictionary containing information about the sent message. If
        error, returns None.
    """

    # Send the SQS message
    sqs_client = boto3.client("sqs")
    try:
        msg = sqs_client.send_message(QueueUrl=sqs_queue_url, MessageBody=msg_body)
    except ClientError as e:
        logging.error(e)
        return None
    return msg


@as_command
def run(*, config_path: str):
    """Exercise send_sqs_message()"""

    # Assign this value before running the program
    c = loading.loadfile(config_path)
    sqs_queue_url = c["QueueUrl"]

    # Set up logging
    logging.basicConfig(
        level=logging.DEBUG, format="%(levelname)s: %(asctime)s: %(message)s"
    )

    # Send some SQS messages
    for i in range(1, 6):
        msg_body = f"SQS message #{i}"
        msg = send_sqs_message(sqs_queue_url, msg_body)
        if msg is not None:
            logging.info(f'Sent SQS message ID: {msg["MessageId"]}')
