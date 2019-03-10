# title of document

summary of subchanges from `old`:

- new property: [`xxx`](#xxx)

---

## section

sentence

Summary of HAR object types:

- [log](#log)
  - [entries](#entries)
    - [response](#response)
      - [query](#query)
      - [postData](#postData)
        - [params](#params)

---

### response

This object contains detailed info about the response.

```json
"response": {
  "status": 200,
  "statusText": "OK",
  "httpVersion": "HTTP/1.1",
  "cookies": [],
  "headers": [],
  "content": {},
  "redirectURL": "",
  "headersSize" : 160,
  "bodySize" : 850,
  "comment" : ""
}
```

| name                      | type      | required  | default | description                                                                                                               |
| ------------------------- | --------- | --------- | ------- | ------------------------------------------------------------------------------------------------------------------------- |
| **status**                | `number`  | ✔️        | `N/A`   | [Response status][rfc7231-status]                                                                                         |
| **statusText**            | `string`  | ✔️        | `N/A`   | [Response status description][rfc7231-status]                                                                             |
| **httpVersion**           | `string`  | ✔️        | `N/A`   | [Response HTTP Version][rfc7230-version]                                                                                  |
| [**cookies**](#cookies)   | `array`   | ✔️        | `N/A`   | List of [`cookie`](#cookies) objects                                                                                      |
| [**headers**](#headers)   | `array`   | ✔️        | `N/A`   | List of [`header`](#headers) objects                                                                                      |
| [**content**](#content)   | `object`  | ✔️        | `N/A`   | Details about the response body                                                                                           |
| **redirectURL**           | `string`  | ✔️        | `N/A`   | Redirection target URL from the Location response header                                                                  |
| **headersSize**           | `number`  | ✔️        | `-1`    | Total number of bytes from the start of the HTTP response message until (and including) the double `CRLF` before the body |
| **headersCompression**    | `number`  | ✖️        | `N/A`   | *(new in 1.3)* Number of bytes saved (for [`HTTP/2`][rfc7540-headers])                                                    |
| **bodySize**              | `number`  | ✔️        | `-1`    | Size of the received response body in bytes. Set to `0` in case of responses coming from the cache (`304`)                |
| **comment**               | `string`  | ✖️        | `N/A`   | A comment provided by the user or the application                                                                         |

> - `headersSize`: The size of received response-headers is computed only from headers that are really received from the server. Additional headers appended by the browser are not included in this number, but they appear in the list of header objects.
> - The total response size received can be computed as follows *(if both values are available)*:
> ```js
> let totalSize = entry.response.headersSize + entry.response.bodySize
> ```


---

### queryString

This object contains list of all parameters & values parsed from a query string, if any (embedded in [`request`](#request) object).

```json
"queryString": [
  {
    "name": "param1",
    "value": "value1",
    "comment": ""
  },
  {
    "name": "param1",
    "value": "value1",
    "comment": ""
  }
]
```

| name          | type      | required  | default | description                                                        |
| ------------- | --------- | --------- | ------- | ------------------------------------------------------------------ |
| **name**      | `string`  | ✔️        | `N/A`   | The name of the query                                              |
| **value**     | `string`  | ✔️        | `N/A`   | The query value                                                    |
| **comment**   | `string`  | ✖️        | `N/A`   | A comment provided by the user or the application                  |


---

### postData

This object describes posted data, if any (embedded in [`request`](#request) object).

```json
"postData": {
  "mimeType": "multipart/form-data",
  "params": [],
  "text" : "plain posted data",
  "comment": ""
}
```

| name                  | type      | required  | default | description                                                          |
| --------------------- | --------- | --------- | ------- | -------------------------------------------------------------------- |
| **mimeType**          | `string`  | ✔️        | `N/A`   | [Mime type][mime-type] of posted data                                |
| [**params**](#params) | `array`   | ✔️ *      | `N/A`   | List of posted parameters (in case of URL encoded parameters)        |
| **text**              | `string`  | ✔️ *      | `N/A`   | Plain text posted data                                               |
| **encoding**          | `string`  | ✖️        | `N/A`   | *(new in 1.3)* - Encoding used for request `text` field e.g "base64" |
| **comment**           | `string`  | ✖️        | `N/A`   | A comment provided by the user or the application                    |

> - `text` and `params` fields are mutually exclusive.
> - `encoding` field is useful for including binary responses (e.g. images) into the HAR file.

###### Example

> **original**

> ```
> [binary data]
> ```

> **encoded**
> ```json
> "postData": {
>   "mimeType": "image/jpeg",
>   "text": "W2JpbmFyeSBkYXRhXQ==",
>   "encoding": "base64",
>   "comment": ""
> }
> ```

--

## Compression

Compression of the HAR file is not part of the core HAR spec. However, in order to store HAR files more efficiently, it is recommended that you compress HAR files on disk (you might want to use `*.zhar` extension for zipped HAR files).

An application supporting HAR, is not required to support compressed HAR files. If the application doesn't support compressed HAR then it's the responsibility of the user to decompress before passing the HAR file into it.

[HTTP Compression][http-compression] is one of the best practices how to speed up web applications and it's also recommended for HAR files.

[http-compression]: http://en.wikipedia.org/wiki/HTTP_compression
