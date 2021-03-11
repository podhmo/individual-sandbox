POST /articles HTTP/1.1
Host: example.net
User-Agent: Go-http-client/1.1
Content-Length: 34
Content-Type: application/json
Accept-Encoding: gzip

{"content": "Some useful content"}
----------------------------------------

HTTP/1.1 400 Bad Request
Connection: close
Content-Type: application/json; charset=utf-8

{"status":"fail","data":{"title":"A title is required"},"message":"bad request"}
