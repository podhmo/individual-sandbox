| tags[0] | method | path | summary |
| :--- | :--- | :--- | :--- |
| Redirects |  GET | /absolute-redirect/{n} | Absolutely 302 Redirects n times. |
| Anything |  DELETE | /anything | Returns anything passed in request data. |
| Anything |  GET | /anything | Returns anything passed in request data. |
| Anything |  PATCH | /anything | Returns anything passed in request data. |
| Anything |  POST | /anything | Returns anything passed in request data. |
| Anything |  PUT | /anything | Returns anything passed in request data. |
| Anything |  TRACE | /anything | Returns anything passed in request data. |
| Anything |  DELETE | /anything/{anything} | Returns anything passed in request data. |
| Anything |  GET | /anything/{anything} | Returns anything passed in request data. |
| Anything |  PATCH | /anything/{anything} | Returns anything passed in request data. |
| Anything |  POST | /anything/{anything} | Returns anything passed in request data. |
| Anything |  PUT | /anything/{anything} | Returns anything passed in request data. |
| Anything |  TRACE | /anything/{anything} | Returns anything passed in request data. |
| Dynamic data |  GET | /base64/{value} | Decodes base64url-encoded string. |
| Auth |  GET | /basic-auth/{user}/{passwd} | Prompts the user for authorization using HTTP Basic Auth. |
| Auth |  GET | /bearer | Prompts the user for authorization using bearer authentication. |
| Response formats |  GET | /brotli | Returns Brotli-encoded data. |
| Dynamic data |  GET | /bytes/{n} | Returns n random bytes generated with given seed |
| Response inspection |  GET | /cache | Returns a 304 if an If-Modified-Since header or If-None-Match is present. Returns the same as a GET otherwise. |
| Response inspection |  GET | /cache/{value} | Sets a Cache-Control header for n seconds. |
| Cookies |  GET | /cookies | Returns cookie data. |
| Cookies |  GET | /cookies/delete | Deletes cookie(s) as provided by the query string and redirects to cookie list. |
| Cookies |  GET | /cookies/set | Sets cookie(s) as provided by the query string and redirects to cookie list. |
| Cookies |  GET | /cookies/set/{name}/{value} | Sets a cookie and redirects to cookie list. |
| Response formats |  GET | /deflate | Returns Deflate-encoded data. |
| Dynamic data |  DELETE | /delay/{delay} | Returns a delayed response (max of 10 seconds). |
| Dynamic data |  GET | /delay/{delay} | Returns a delayed response (max of 10 seconds). |
| Dynamic data |  PATCH | /delay/{delay} | Returns a delayed response (max of 10 seconds). |
| Dynamic data |  POST | /delay/{delay} | Returns a delayed response (max of 10 seconds). |
| Dynamic data |  PUT | /delay/{delay} | Returns a delayed response (max of 10 seconds). |
| Dynamic data |  TRACE | /delay/{delay} | Returns a delayed response (max of 10 seconds). |
| HTTP Methods |  DELETE | /delete | The request's DELETE parameters. |
| Response formats |  GET | /deny | Returns page denied by robots.txt rules. |
| Auth |  GET | /digest-auth/{qop}/{user}/{passwd} | Prompts the user for authorization using Digest Auth. |
| Auth |  GET | /digest-auth/{qop}/{user}/{passwd}/{algorithm} | Prompts the user for authorization using Digest Auth + Algorithm. |
| Auth |  GET | /digest-auth/{qop}/{user}/{passwd}/{algorithm}/{stale_after} | Prompts the user for authorization using Digest Auth + Algorithm. |
| Dynamic data |  GET | /drip | Drips data over a duration after an optional initial delay. |
| Response formats |  GET | /encoding/utf8 | Returns a UTF-8 encoded body. |
| Response inspection |  GET | /etag/{etag} | Assumes the resource has the given etag and responds to If-None-Match and If-Match headers appropriately. |
| HTTP Methods |  GET | /get | The request's query parameters. |
| Response formats |  GET | /gzip | Returns GZip-encoded data. |
| Request inspection |  GET | /headers | Return the incoming request's HTTP headers. |
| Auth |  GET | /hidden-basic-auth/{user}/{passwd} | Prompts the user for authorization using HTTP Basic Auth. |
| Response formats |  GET | /html | Returns a simple HTML document. |
| Images |  GET | /image | Returns a simple image of the type suggest by the Accept header. |
| Images |  GET | /image/jpeg | Returns a simple JPEG image. |
| Images |  GET | /image/png | Returns a simple PNG image. |
| Images |  GET | /image/svg | Returns a simple SVG image. |
| Images |  GET | /image/webp | Returns a simple WEBP image. |
| Request inspection |  GET | /ip | Returns the requester's IP Address. |
| Response formats |  GET | /json | Returns a simple JSON document. |
| Dynamic data |  GET | /links/{n}/{offset} | Generate a page containing n links to other pages which do the same. |
| HTTP Methods |  PATCH | /patch | The request's PATCH parameters. |
| HTTP Methods |  POST | /post | The request's POST parameters. |
| HTTP Methods |  PUT | /put | The request's PUT parameters. |
| Dynamic data |  GET | /range/{numbytes} | Streams n random bytes generated with given seed, at given chunk size per packet. |
| Redirects |  DELETE | /redirect-to | 302/3XX Redirects to the given URL. |
| Redirects |  GET | /redirect-to | 302/3XX Redirects to the given URL. |
| Redirects |  PATCH | /redirect-to | 302/3XX Redirects to the given URL. |
| Redirects |  POST | /redirect-to | 302/3XX Redirects to the given URL. |
| Redirects |  PUT | /redirect-to | 302/3XX Redirects to the given URL. |
| Redirects |  TRACE | /redirect-to | 302/3XX Redirects to the given URL. |
| Redirects |  GET | /redirect/{n} | 302 Redirects n times. |
| Redirects |  GET | /relative-redirect/{n} | Relatively 302 Redirects n times. |
| Response inspection |  GET | /response-headers | Returns a set of response headers from the query string. |
| Response inspection |  POST | /response-headers | Returns a set of response headers from the query string. |
| Response formats |  GET | /robots.txt | Returns some robots.txt rules. |
| Status codes |  DELETE | /status/{codes} | Return status code or random status code if more than one are given |
| Status codes |  GET | /status/{codes} | Return status code or random status code if more than one are given |
| Status codes |  PATCH | /status/{codes} | Return status code or random status code if more than one are given |
| Status codes |  POST | /status/{codes} | Return status code or random status code if more than one are given |
| Status codes |  PUT | /status/{codes} | Return status code or random status code if more than one are given |
| Status codes |  TRACE | /status/{codes} | Return status code or random status code if more than one are given |
| Dynamic data |  GET | /stream-bytes/{n} | Streams n random bytes generated with given seed, at given chunk size per packet. |
| Dynamic data |  GET | /stream/{n} | Stream n JSON responses |
| Request inspection |  GET | /user-agent | Return the incoming requests's User-Agent header. |
| Dynamic data |  GET | /uuid | Return a UUID4. |
| Response formats |  GET | /xml | Returns a simple XML document. |
