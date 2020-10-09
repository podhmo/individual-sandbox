---
title: Sample API v0.0.0
language_tabs:
  - shell: Shell
  - http: HTTP
  - javascript: JavaScript
  - ruby: Ruby
  - python: Python
  - php: PHP
  - java: Java
  - go: Go
toc_footers: []
includes: []
search: true
highlight_theme: darkula
headingLevel: 2

---

Template name: main
Stage: pre
<!-- Generator: Widdershins v4.0.1 -->

<h1 id="sample-api">Sample API v0.0.0</h1>

> Scroll down for code samples, example requests and responses. Select a language for code samples from the tabs above or the mobile navigation menu.

-

Base URLs:

* <a href="http://localhost:8888">http://localhost:8888</a>

<h1 id="sample-api-default">Default</h1>

## main.ListUser

<a id="opIdmain.ListUser"></a>

`GET /users`

<h3 id="main.listuser-parameters">Parameters</h3>

|Name|In|Type|Required|Description|
|---|---|---|---|---|
|limit|query|integer|true|none|

> Example responses

> 200 Response

```json
{
  "items": {
    "additionalProperties": false,
    "properties": {
      "age": {
        "type": "string"
      },
      "id": {
        "type": "integer"
      },
      "name": {
        "type": "string"
      }
    },
    "required": [
      "id",
      "name"
    ],
    "type": "object"
  },
  "type": "array"
}
```

<h3 id="main.listuser-responses">Responses</h3>

|Status|Meaning|Description|Schema|
|---|---|---|---|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|Inline|
|default|Default|none|None|

<h3 id="main.listuser-responseschema">Response Schema</h3>

Status Code **200**

|Name|Type|Required|Restrictions|Description|
|---|---|---|---|---|
|*anonymous*|[[User](#schemauser)]|false|none|none|
|» age|string|false|none|none|
|» id|integer|true|none|none|
|» name|string|true|none|none|

<aside class="success">
This operation does not require authentication
</aside>

## main.InsertUser

<a id="opIdmain.InsertUser"></a>

`POST /users`

> Body parameter

```json
{
  "additionalProperties": false,
  "properties": {
    "age": {
      "type": "string"
    },
    "id": {
      "type": "integer"
    },
    "name": {
      "type": "string"
    }
  },
  "required": [
    "id",
    "name"
  ],
  "type": "object"
}
```

<h3 id="main.insertuser-parameters">Parameters</h3>

|Name|In|Type|Required|Description|
|---|---|---|---|---|
|body|body|[InsertUserInput](#schemainsertuserinput)|false|none|

> Example responses

> 200 Response

```json
{
  "additionalProperties": false,
  "properties": {
    "age": {
      "type": "string"
    },
    "id": {
      "type": "integer"
    },
    "name": {
      "type": "string"
    }
  },
  "required": [
    "id",
    "name"
  ],
  "type": "object"
}
```

<h3 id="main.insertuser-responses">Responses</h3>

|Status|Meaning|Description|Schema|
|---|---|---|---|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|[User](#schemauser)|
|default|Default|none|None|

<aside class="success">
This operation does not require authentication
</aside>

## main.FindUser

<a id="opIdmain.FindUser"></a>

`GET /users/{userId}`

<h3 id="main.finduser-parameters">Parameters</h3>

|Name|In|Type|Required|Description|
|---|---|---|---|---|
|userId|path|integer|true|none|

> Example responses

> 200 Response

```json
{
  "additionalProperties": false,
  "properties": {
    "age": {
      "type": "string"
    },
    "id": {
      "type": "integer"
    },
    "name": {
      "type": "string"
    }
  },
  "required": [
    "id",
    "name"
  ],
  "type": "object"
}
```

<h3 id="main.finduser-responses">Responses</h3>

|Status|Meaning|Description|Schema|
|---|---|---|---|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|[User](#schemauser)|
|default|Default|none|None|

<aside class="success">
This operation does not require authentication
</aside>

## main.PutUser

<a id="opIdmain.PutUser"></a>

`PUT /users/{userId}`

> Body parameter

```json
{
  "additionalProperties": false,
  "properties": {
    "age": {
      "type": "string"
    },
    "id": {
      "type": "integer"
    },
    "name": {
      "type": "string"
    }
  },
  "required": [
    "id",
    "name"
  ],
  "type": "object"
}
```

<h3 id="main.putuser-parameters">Parameters</h3>

|Name|In|Type|Required|Description|
|---|---|---|---|---|
|userId|path|integer|true|none|
|body|body|[PutUserInput](#schemaputuserinput)|false|none|

> Example responses

> 200 Response

```json
{
  "additionalProperties": false,
  "properties": {
    "age": {
      "type": "string"
    },
    "id": {
      "type": "integer"
    },
    "name": {
      "type": "string"
    }
  },
  "required": [
    "id",
    "name"
  ],
  "type": "object"
}
```

<h3 id="main.putuser-responses">Responses</h3>

|Status|Meaning|Description|Schema|
|---|---|---|---|
|200|[OK](https://tools.ietf.org/html/rfc7231#section-6.3.1)|none|[User](#schemauser)|
|default|Default|none|None|

<aside class="success">
This operation does not require authentication
</aside>

# Schemas

<h2 id="tocS_InsertUserInput">InsertUserInput</h2>
<!-- backwards compatibility -->
<a id="schemainsertuserinput"></a>
<a id="schema_InsertUserInput"></a>
<a id="tocSinsertuserinput"></a>
<a id="tocsinsertuserinput"></a>

```json
{
  "additionalProperties": false,
  "properties": {
    "age": {
      "type": "string"
    },
    "id": {
      "type": "integer"
    },
    "name": {
      "type": "string"
    }
  },
  "required": [
    "id",
    "name"
  ],
  "type": "object"
}

```

### Properties

|Name|Type|Required|Restrictions|Description|
|---|---|---|---|---|
|age|string|false|none|none|
|id|integer|true|none|none|
|name|string|true|none|none|

<h2 id="tocS_PutUserInput">PutUserInput</h2>
<!-- backwards compatibility -->
<a id="schemaputuserinput"></a>
<a id="schema_PutUserInput"></a>
<a id="tocSputuserinput"></a>
<a id="tocsputuserinput"></a>

```json
{
  "additionalProperties": false,
  "properties": {
    "age": {
      "type": "string"
    },
    "id": {
      "type": "integer"
    },
    "name": {
      "type": "string"
    }
  },
  "required": [
    "id",
    "name"
  ],
  "type": "object"
}

```

### Properties

|Name|Type|Required|Restrictions|Description|
|---|---|---|---|---|
|age|string|false|none|none|
|id|integer|true|none|none|
|name|string|true|none|none|

<h2 id="tocS_User">User</h2>
<!-- backwards compatibility -->
<a id="schemauser"></a>
<a id="schema_User"></a>
<a id="tocSuser"></a>
<a id="tocsuser"></a>

```json
{
  "additionalProperties": false,
  "properties": {
    "age": {
      "type": "string"
    },
    "id": {
      "type": "integer"
    },
    "name": {
      "type": "string"
    }
  },
  "required": [
    "id",
    "name"
  ],
  "type": "object"
}

```

### Properties

|Name|Type|Required|Restrictions|Description|
|---|---|---|---|---|
|age|string|false|none|none|
|id|integer|true|none|none|
|name|string|true|none|none|

Template name: main
Stage: post
