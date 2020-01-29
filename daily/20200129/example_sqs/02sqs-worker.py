import time
import threading
from functools import partial
import logging
import boto3
from botocore.exceptions import ClientError
from dictknife import loading
from handofcats import as_command

logger = logging.getLogger(__name__)


@as_command
def run(*, queue_url: str):
    def provider(queue_url: str):
        sqs_client = boto3.client("sqs")
        try:
            for i in range(1, 10):
                m = f"SQS message #{i}"
                sqs_client.send_message(QueueUrl=queue_url, MessageBody=m)
                logger.info(f"-> {m}")
                time.sleep(0.1)
        except ClientError as e:
            logger.info("!!", e)

    def provider2(queue_url: str):
        sqs_client = boto3.client("sqs")
        try:
            for n in range(3):
                entries = []
                for i in range(n * 6, (n + 1) * 6):
                    entries.append(
                        {
                            "Id": f"m{i}",
                            "MessageBody": f"SQS message2 #{i}",
                            "DelaySeconds": 1,
                        }
                    )
                if not entries:
                    return
                logger.info(f"=> {', '.join(x['MessageBody'] for x in entries)}")
                # max: 10
                _ = sqs_client.send_message_batch(QueueUrl=queue_url, Entries=entries)
                time.sleep(0.3)
        except ClientError as e:
            logger.info("!!", e)

    def consumer(queue_url: str, *, ev: threading.Event):
        sqs_client = boto3.client("sqs")
        try:
            while True:
                response = sqs_client.receive_message(
                    QueueUrl=queue_url,
                    AttributeNames=["SentTimestamp"],
                    MaxNumberOfMessages=10,
                    MessageAttributeNames=["All"],
                    WaitTimeSeconds=5,
                    VisibilityTimeout=1,
                )
                # logger.info(loading.dumpfile(response))
                ev.set()
                # logger.info("----------------------------------------")
                try:
                    messages = response["Messages"]
                except KeyError:
                    logger.info("EMPTY")
                    return

                # logger.info(len(messages))

                will_be_deleted = []
                logger.info(f"<= {', '.join(msg['Body'] for msg in messages)}")
                for msg in messages:
                    will_be_deleted.append(
                        {"Id": msg["MessageId"], "ReceiptHandle": msg["ReceiptHandle"]}
                    )

                _ = sqs_client.delete_message_batch(
                    QueueUrl=queue_url, Entries=will_be_deleted
                )
        except ClientError as e:
            logger.info("!!", e)

    ev = threading.Event()
    threads = []
    threads.append(threading.Thread(target=partial(provider, queue_url)))
    threads.append(threading.Thread(target=partial(provider2, queue_url)))
    threads.append(threading.Thread(target=partial(consumer, queue_url, ev=ev)))
    for th in threads:
        th.start()
    ev.wait()
    for th in threads:
        th.join()
    logger.info("ok")
