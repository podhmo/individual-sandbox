from handofcats import as_command
from google.cloud import pubsub_v1


@as_command
def run(*, topic: str, project_id: str):
    # NEED GOOGLE_APPLICATION_CREDENTIALS
    # TODO project_id = "Your Google Cloud Project ID"
    # TODO topic_name = "Your Pub/Sub topic name"

    publisher = pubsub_v1.PublisherClient()
    topic_name = "projects/{project_id}/topics/{topic}".format(
        project_id=project_id, topic=topic,  # Set this to something appropriate.
    )

    def ok(fut):
        print("published: ", fut.result())

    fut = publisher.publish(topic_name, b"My first message!", spam="eggs")
    fut.add_done_callback(ok)
    import time

    time.sleep(2)
    print("hoi")
