import os
import sys
from wsgiref.simple_server import make_server

PORT = int(os.getenv("PORT") or "8080")


def app(environ, start_response):
    path = environ["PATH_INFO"]
    if path == "/sse":
        yield from sse(environ, start_response)
    else:
        yield from index(environ, start_response)


def index(environ, start_response):
    status = "200 OK"
    headers = [
        ("Content-type", "text/html; charset=utf-8"),
    ]
    start_response(status, headers)
    yield f"""
<body>
  <script>
    // const evtSource = new EventSource("//localhost:{PORT}/sse", {{ withCredentials: true }} ); 
    const evtSource = new EventSource("//localhost:{PORT}/sse", {{ withCredentials: false }} ); 
    evtSource.onmessage = function(e) {{
      var newElement = document.createElement("li");
      var eventList = document.getElementById('list');

      newElement.innerHTML = "message: " + e.data;
      eventList.appendChild(newElement);
    }}
    // if evtSource.close() is not called, reset connection
  </script>
<body>
  <div id="list"></div>
</body>
</body>
""".encode(
        "utf-8"
    )


def sse(environ, start_response):
    import time

    status = "200 OK"
    headers = [
        ("Content-type", "text/event-stream; charset=utf-8"),
        ("Cache-Control", "no-cache"),
    ]
    start_response(status, headers)
    yield ": this is a test stream".encode("utf-8")  # this is comment
    yield "\n".encode("utf-8")

    for i in range(10):
        yield f'data: {{"time": "{time.time()}", "c": {i} }}\n'.encode("utf-8")
        yield "\n".encode("utf-8")
        time.sleep(0.5)

    yield "event: exit\n".encode("utf-8")


httpd = make_server("", PORT, app)
print(f"Serving on port {PORT}...", file=sys.stderr)

# Serve until process is killed
httpd.serve_forever()
