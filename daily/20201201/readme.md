## go advent calendar 2020

埋め込み本当に必要？

- new typeのときの挙動 https://golang.org/ref/spec#Type_declarations

  - methodは初期化
  - フィールドはそのまま（それはそう）

- heapに格納されるオブジェクトの数が減る


## terraformてきなあれこれ

resourceがあるかのようにして指定できるのは便利だよなー。

## serverless

なんかexamplesが存在する。

- https://github.com/serverless/examples/tree/master/aws-golang-http-get-post

API gateway用のresponseの定義もあったりするんだ。aws-lambda-go。

- https://github.com/aws/aws-lambda-go/blob/master/events/README_ApiGatewayEvent.md

## aws lambda api-gateway

そもそもこれはなに？

- https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/getting-started-with-private-integration.html

どうやるの？

- https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-lambda-custom-integrations.html
- https://docs.aws.amazon.com/ja_jp/lambda/latest/dg/services-apigateway-tutorial.html
- https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/api-gateway-create-api-step-by-step.html

どうもいろいろ調べたけどわかんないのは、 HTTP APIっぽい。

> HTTP API または REST API を使用して RESTful API を作成します。HTTP API は、ほとんどのユースケースで API を構築する最適な方法です。REST API より最大 71% 費用が安くなります。

proxy integrationのほうが手軽そう。

### aws api-gateway がわかんない

- REST API integration, HTTP integration

  - これはHTTP integrationに順次移行していく形だったと思う
  - ただし、REST API integrationでなければできないこともまだある
  - https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/http-api-vs-rest.html

- proxy integration

  - proxy integration  https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/api-gateway-create-api-as-simple-proxy-for-http.html
  - not proxy integration https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/api-gateway-create-api-step-by-step.html

- private integration

  - https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/getting-started-with-private-integration.html


proxyの方は、なんか特定のserviceのforwardingする感じっぽい。
いや、意味がわかった。edge proxy的な話そう。REST APIの話じゃん。
private integrationは外から中に入る時に、vpc内のやつにアクセスできるみたいな感じ？
vpc linkを作ってそれとつながるlbが必要っぽい。

- https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/http-api-open-api.html#http-api-import

### quick create

なんかawscli上からquick createと言う機能で、1:1にmappingされるAPIは手軽に作れるっぽい。

- https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/http-api-develop.html

### routes, integrations

- quick create? -> ANY `/` にbind? -> default stageか。
- ruote -> stage

この辺に答えがあった。 https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-routes.html



## cicd のworkshop?


- https://cicd.serverlessworkshops.io/javascript
