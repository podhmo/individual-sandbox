from handofcats import as_command
from dictknife import loading


@as_command
def run(*, config_path: str) -> None:
    c = loading.loadfile(config_path)
    queue_url = c["QueueUrl"]
    print(queue_url)
