GET / HTTP/1.1
Host: example.net
User-Agent: Go-http-client/1.1
Accept-Encoding: gzip


----------------------------------------

HTTP/1.1 500 Internal Server Error
Connection: close
Content-Type: application/json; charset=utf-8

{"status":"error","message":"Unable to communicate with database"}
