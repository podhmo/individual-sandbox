import sys
import requests
import signal

token = object()


def on_terminate(signum, tb):
    global token
    print("@", token)
    token = None  # shutdown
    print("@@", token)
    sys.exit(0)


signal.signal(signal.SIGINT, on_terminate)
response = requests.get("http://localhost:8000")
token = None
print(response.text)
