import time
import pickle

d = {
    "request": {
        "body": None,
        "headers": {
            "Accept": "*/*",
            "Accept-Encoding": "gzip, deflate",
            "Connection": "keep-alive",
            "User-Agent": "python-requests/2.18.4"
        },
        "method": "GET",
        "url": "http://localhost:4444/"
    },
    "response": {
        "body": {
            "accept": "*/*",
            "accept_encoding": "gzip, deflate",
            "connection": "keep-alive",
            "content_length": "",
            "content_type": "text/plain",
            "host": "localhost:4444",
            "method": "GET",
            "path": "/",
            "querystring": "",
            "user_agent": "python-requests/2.18.4"
        },
        "headers": {
            "Content-Length": "248",
            "Content-type": "application/pickle; charset=utf-8",
            "Date": "Sun, 17 Dec 2017 10:30:45 GMT",
            "Server": "WSGIServer/0.2 CPython/3.6.2"
        },
        "status_code": 200
    }
}

N = 10000
st = time.time()
for i in range(N):
    pickle.dumps(d)
print(time.time() - st)
