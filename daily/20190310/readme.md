## swagger 2 md

これのフォーマットが良い気がした。

- https://github.com/ahmadnassri/har-spec/blob/master/versions/1.3.md

どういう情報が表示できるんだろう？

```markdown
# ドキュメントのタイトル

ここに本文(Changes)とか書く。 from `old` :

- new property: [`xxx`](#xxx)

```

# ドキュメントのタイトル

ここに本文(Changes)とか書く。 from `old` :

- new property: [`xxx`](#xxx)

> サブセクション部分。`---`で区切る

```markdown
## サブタイトル

ここに文章。

struct的なrefがあった場合にここに書く。

Summary of XXX object types:

- [xxx](#xxx)
  - [subType](#subType)
```

## サブタイトル

ここに文章。

struct的なrefがあった場合にここに書く。

Summary of XXX object types:

- [xxx](#xxx)
  - [subType](#subType)

> 更にサブサブセクション。手元にすぐにexampleがある。ここどうしようかな？

```markdown
### サブサブセクション

<json data>
```

表は以下の様な形式

```
| name                      | type      | required  | default | description                                                                                                               |
| ------------------------- | --------- | --------- | ------- | ------------------------------------------------------------------------------------------------------------------------- |
| **status**                | `number`  | ✔️        | `N/A`   | [Response status][rfc7231-status]                                                        |
| [**cookies**](#cookies)   | `array`   | ✔️        | `N/A`   | List of [`cookie`](#cookies) objects                                                                                      |
```

| name                      | type      | required  | default | description                                                                                                               |
| ------------------------- | --------- | --------- | ------- | ------------------------------------------------------------------------------------------------------------------------- |
| **status**                | `number`  | ✔️        | `N/A`   | [Response status][rfc7231-status]                                                        |
| [**cookies**](#cookies)   | `array`   | ✔️        | `N/A`   | List of [`cookie`](#cookies) objects                                                                                      |

更に細かな注釈をする時には引用的なものフォーマットを使っている。

```markdown
> - `headersSize`: The size of received response-headers is computed only from headers that are really received from the server. Additional headers appended by the browser are not included in this number, but they appear in the list of header objects.
> - The total response size received can be computed as follows *(if both values are available)*:
> ```js
> let totalSize = entry.response.headersSize + entry.response.bodySize
> ```
```

> - `headersSize`: The size of received response-headers is computed only from headers that are really received from the server. Additional headers appended by the browser are not included in this number, but they appear in the list of header objects.
> - The total response size received can be computed as follows *(if both values are available)*:
> ```js
> let totalSize = entry.response.headersSize + entry.response.bodySize
> ```

