import requests
import json


def main():
    url = "http://localhost:4000/jsonrpc"
    headers = {'content-type': 'application/json'}
    payload = {
        "method": "Arith.Add",
        "params": [{
            "A": 10,
            "B": 20
        }],
        "jsonrpc": "2.0",
        "id": 0,
    }
    response = requests.post(url, data=json.dumps(payload), headers=headers).json()
    print(response)


if __name__ == "__main__":
    main()
