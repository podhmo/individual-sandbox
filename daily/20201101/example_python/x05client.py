from handofcats import as_command
import requests
import json


@as_command
def run(*, port: int) -> None:
    data = dataset = [
        {"name": "foo"},
        {"nam": "x"},
    ]
    for data in dataset:
        response = requests.post(
            f"http://127.0.0.1:{port}",
            data=json.dumps(data),
            timeout=1,
            headers={"Content-Type": "application/json"},
        )
        print(response, response.text)
