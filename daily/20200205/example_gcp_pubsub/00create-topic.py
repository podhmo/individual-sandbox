from handofcats import as_command
from google.cloud import pubsub_v1


@as_command
def run(*, topic_name: str, project_id: str):
    # NEED GOOGLE_APPLICATION_CREDENTIALS
    # TODO project_id = "Your Google Cloud Project ID"
    # TODO topic_name = "Your Pub/Sub topic name"

    publisher = pubsub_v1.PublisherClient()
    topic_path = publisher.topic_path(project_id, topic_name)

    topic = publisher.create_topic(topic_path)

    print("Topic created: {}".format(topic))
