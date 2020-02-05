import os
from google.cloud import pubsub_v1
from handofcats import as_command


@as_command
def run(*, topic: str, project_id: str, sub: str):
    # NEED GOOGLE_APPLICATION_CREDENTIALS
    # TODO project_id = "Your Google Cloud Project ID"
    # TODO topic = "Your Pub/Sub topic name"

    # publisher = pubsub_v1.PublisherClient()
    subscriber = pubsub_v1.SubscriberClient()
    topic_name = "projects/{project_id}/topics/{topic}".format(
        project_id=project_id, topic=topic,
    )
    subscription_name = "projects/{project_id}/subscriptions/{sub}".format(
        project_id=project_id, sub=sub,
    )
    # subscriber.create_subscription(name=subscription_name, topic=topic_name)

    def callback(message):
        print(message.data)
        message.ack()

    future = subscriber.subscribe(subscription_name, callback)
    import time

    time.sleep(2)
    print("@", future.result())
