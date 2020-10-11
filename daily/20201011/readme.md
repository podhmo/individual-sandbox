## aws role

- https://policysim.aws.amazon.com/

## aws role管理

terraformでやる？

## aws api gateway openapi

- https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/http-api-open-api.html

```
$ grep 'x-' example_api_gateway/openapi.json | sort -u | sed 's/^ *//g'
"x-amazon-apigateway-integration": {
"x-amz-date",
"x-amzm-header",
"x-api-key",
"x-apigateway-header",
"x-amazon-apigateway-cors": {
```

これらを設定しないとだめ？


### 利用方法

> HTTP API は、OpenAPI 3.0 定義ファイルをインポートすることによって作成できます。
> REST API から HTTP API に移行するには、REST API を OpenAPI 3.0 定義ファイルとしてエクスポートできます。

読む限り、importして使えるのはともかくexportして使うんだろうか？

```
aws apigatewayv2 import-api --body file://api-definition.json

...
{
    "ApiEndpoint": "https://6lx1lvrc5k.execute-api.ap-northeast-1.amazonaws.com",
    "ApiId": "6lx1lvrc5k",
    "ApiKeySelectionExpression": "$request.header.x-api-key",
    "CorsConfiguration": {
        "AllowHeaders": [
            "x-amzm-header",
            "x-apigateway-header",
            "x-api-key",
            "authorization",
            "x-amz-date",
            "content-type"
        ],
        "AllowMethods": [
            "GET",
            "OPTIONS",
            "POST"
        ],
        "AllowOrigins": [
            "*"
        ]
    },
    "CreatedDate": "2020-10-11T10:26:36Z",
    "Description": "A Pet Store API.",
    "DisableExecuteApiEndpoint": false,
    "ImportInfo": [
        "components: key [schemas] will be ignored by API Gateway",
        "paths./pets.post: key [requestBody] will be ignored by API Gateway",
        "paths./pets.post: key [responses] will be ignored by API Gateway",
        "paths./pets.get: key [responses] will be ignored by API Gateway",
        "paths./pets.get: key [parameters] will be ignored by API Gateway",
        "paths./pets/{petId}.get: key [responses] will be ignored by API Gateway",
        "paths./pets/{petId}.get: key [parameters] will be ignored by API Gateway"
    ],
    "Name": "Example Pet Store",
    "ProtocolType": "HTTP",
    "RouteSelectionExpression": "$request.method $request.path",
    "Tags": {},
    "Version": "1.0",
    "Warnings": [
        "Unable to create integration for resource at path 'GET /pets': Unsupported PayloadFormatVersion: 1. Ignoring.",
        "Unable to create integration for resource at path 'POST /pets': Unsupported PayloadFormatVersion: 1. Ignoring.",
        "Unable to create integration for resource at path 'GET /pets/{petId}': Unsupported PayloadFormatVersion: 1. Ignoring."
    ]
}
```

↑をimportしてみたら、pathsだけが登録される感じになった。

まじめに、api gatewayのサンプル動かしてみたほうが良い気もする。

### tutorial

- https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/api-gateway-tutorials.html

- s3のproxy https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/integrating-api-with-aws-services-s3.html


- hmm https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/api-gateway-create-api-as-simple-proxy-for-http.html
- https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/getting-started-with-private-integration.html

あとで消す

- vpc
- api-gateway

api-gatewayの認証で軽いものってなんなんだろう？lambdaより軽いものがある気がする？
iam認証はcognito経由なのか。openID経由というのはgsuiteのやつもいけるんだろうか？

- https://medium.com/@alsmola/alb-authentication-with-g-suite-saml-using-cognito-858e35564dc8

https://medium.com/@Da_vidgf/http-basic-auth-with-api-gateway-and-serverless-5ae14ad0a270

## aws 明細


awscliで見れたりしたら便利では？

```console
$ aws sts get-caller-identity
{
    "UserId": "XXXXxXXXxXXXXXXXXxXXX",
    "Account": "xxxxxxxxxxxx",
    "Arn": "arn:aws:iam::784330574880:user/xxxxxxxx"
}
$ aws budgets describe-budgets --output table --account-id $(aws sts get-caller-identity | jq .Account -r)
```

しかし、ほしいのはbudgetsではないな。。

```
$ aws ce get-cost-and-usage --output table --time-period Start=$(gdate '+%Y-%m')-01,End=$(gdate -d 'next month' '+%Y-%m')-01 --metrics BlendedCost --granularity MONTHLY --group-by Type=DIMENSION,Key=SERVICE
```

こんな感じ？

- https://docs.aws.amazon.com/cli/latest/reference/ce/get-cost-and-usage-with-resources.html
- https://docs.aws.amazon.com/cli/latest/reference/ce/get-cost-and-usage-with-resources.html
