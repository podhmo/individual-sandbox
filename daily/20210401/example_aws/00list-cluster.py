import json
from boto3 import Session
from handofcats import as_command


@as_command
def run(*, region: str = "us-east-1") -> None:
    session = Session(
        region_name=region,
    )
    client = session.client("ecs")
    response = client.list_clusters()
    print(json.dumps(response, indent=2, ensure_ascii=False))
