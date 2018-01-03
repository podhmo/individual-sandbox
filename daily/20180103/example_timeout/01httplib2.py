import sys
import httplib2
import signal

token = object()


def on_terminate(signum, tb):
    global token
    print("@", token)
    token = None  # shutdown
    print("@@", token)
    sys.exit(0)


signal.signal(signal.SIGINT, on_terminate)
http = httplib2.Http()
response, body = http.request("http://localhost:8000", "GET")
token = None
print(body)
