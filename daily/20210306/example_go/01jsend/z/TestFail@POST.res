{
  "method": "POST",
  "url": "http://example.net/articles",
  "httpVersion": "HTTP/1.1",
  "headerSize": -1,
  "bodySize": -1,
  "contentType": "application/json",
  "paths": {
    "/articles": {
      "post": {
        "parameters": [
          {
            "name": "Content-Type",
            "in": "header",
            "examples": [
              "application/json"
            ]
          }
        ],
        "requestBody": {
          "content": {
            "application/json": {
              "example": {
                "content": "Some useful content"
              }
            }
          }
        }
      }
    }
  }
}
