import json
from boto3 import Session
from handofcats import as_command


@as_command
def run(*, access_key: str, secret_key: str, region: str) -> None:
    session = Session(
        aws_access_key_id=access_key,
        aws_secret_access_key=secret_key,
        region_name=region,
    )
    client = session.client("ecs")
    response = client.list_clusters()
    print(json.dumps(response, indent=2, ensure_ascii=False))
