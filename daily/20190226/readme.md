## openapi

2to3

```console
$ npm install swagger2openapi
$ ./node_modules/.bin/swagger2openapi <file>
```

validator(lint)

```console
$ npm install speccy
$ ./node_modules/.bin/speccy lint <file>
```

resolveで良い感じにresolveしてくれるんじゃ。


## openapiの変換

oas-kit/swagger2openapiとの差分

- requestBodiesへの変換
- securitySchemesのflowsの変換
- collectionFormatへの対応
- paramters/formDataが変換されていない

  - application/x-www-form-urlencoded
  - multipart/form-data
  - application/octet-stream // file or type=string,format=binary

- headersの部分がそのまま(parametersと同じ感覚でやるべき？）

## openapi collectionFormat

https://github.com/OAI/OpenAPI-Specification/blob/master/versions/2.0.md#parameter-object

collectionFormatというのは

- csv,ssv,tsv,pipes,multi

それぞれがどういうstyleに展開されるか

### OAS3

- explode true/false

```
# true
/hair?color=R,0,G,255,B,128

# false
/hair?R=0&G=255&B=128
```

- inによって使えるstyleが変わる
- explodeとの組み合わせで変わる
- typeによっても変わる

### まじめに理解するか。。

https://github.com/OAI/OpenAPI-Specification/blob/master/versions/3.0.2.md#parameter-object

style(default)

- query - form
- path - simple
- header - simple
- cookie - form

explode (true/false)

allowReserved (RFC3986 :/?#[]@!$&'()*+,;=)

schema

ようやくここでstyle

path

- matrix
- label
- simple

header

- simple

query

- form
- simple
- spaceDelimited
- pipeDelimited
- deepObject

cookie

- form
- simple

| style |  | explode | empty | string | array | object |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| matrix |  | false | ;color | ;color=blue | ;color=blue,black,brown | ;color=R,100,G,200,B,150 |
| matrix |  | true | ;color | ;color=blue | ;color=blue;color=black;color=brown | ;R=100;G=200;B=150 |
| label |  | false | . | .blue | .blue.black.brown | .R.100.G.200.B.150 |
| label |  | true | . | .blue | .blue.black.brown | .R=100.G=200.B=150 |
| form |  | false | color= | color=blue | color=blue,black,brown | color=R,100,G,200,B,150 |
| form |  | true | color= | color=blue | color=blue&color=black&color=brown | R=100&G=200&B=150 |
| simple |  | false | n/a | blue | blue,black,brown | R,100,G,200,B,150 |
| simple |  | true | n/a | blue | blue,black,brown | R=100,G=200,B=150 |
| spaceDelimited |  | false | n/a | n/a | blue%20black%20brown | R%20100%20G%20200%20B%20150 |
| pipeDelimited |  | false | n/a | n/a | blue|black|brown | R|100|G|200 |
| deepObject |  | true | n/a | n/a | n/a | color[R]=100&color[G]=200&color[B]=150 |

## 変換先を

\	object	array
csv	object	Array
ssv	style=spaceDelimited	Array
tsv	object	Array
pipes	style=pipeDelimited	Array
multi	object	Array

tsvはdropされている。(RuntimeError出しておくか)

## converterはある

https://github.com/Mermade/oas-kit
