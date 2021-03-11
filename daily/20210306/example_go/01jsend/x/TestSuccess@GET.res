GET / HTTP/1.1
Host: example.net
User-Agent: Go-http-client/1.1
Accept-Encoding: gzip


----------------------------------------

HTTP/1.1 200 OK
Connection: close
Content-Type: application/json; charset=utf-8

{"status":"success","data":{"posts":[{"id":1,"title":"A blog post","body":"Some useful content"},{"id":2,"title":"Another blog post","body":"More content"}]}}
