## aws s3

意外とversioningを有効にしたりする方法でハマる。あとregion指定。

そしてversioningを有効にしているとbucket空にするのが大変。（削除時に空であることを要求される）


### aio

- https://pypi.org/project/aioboto3/

### sqsへsendするとか

API gatewayになりそうだなー。

- https://qiita.com/horit/items/417c8ab2b4667fe310b9

## aws SAM

- https://docs.aws.amazon.com/ja_jp/serverlessrepo/latest/devguide/using-aws-sam.html
- https://docs.aws.amazon.com/ja_jp/serverless-application-model/latest/developerguide/serverless-getting-started-hello-world.html
- https://docs.aws.amazon.com/serverless-application-model/latest/developerguide/what-is-sam.html

## aws lambda + api gateway + awscli

- とりあえずこれは知っておいたほうが良さそう
- IAM
- awscli上で
- aws cdk?

### cli

- https://docs.aws.amazon.com/ja_jp/lambda/latest/dg/gettingstarted-awscli.html
- AWSLambdaBasicExecutionRole
- https://nmmmk.hatenablog.com/entry/2018/10/10/001548

だいぶIAMがだるい。

- https://dev.classmethod.jp/cloud/aws/aws-cdk-python-ec2/
- https://docs.aws.amazon.com/ja_jp/apigateway/latest/developerguide/create-api-using-awscli.html

## monogusa

もう少し卑近なところからやってみる？いろいろ考えたりもしたけれど。

### 雑なつぶやき

- 便利さという意味でいうと、background taskよりもgcsやs3への保存と通知のほうが大切？
- 設定をしたくない手軽に使えるだけでなく、設定を剥がすのが面倒という点も重要
- 非同期タスクがほしいだけ。これもmanager processを圧迫させたくないだけ

どういう気持ちを重視しているか

- 手元でできることを他のところでも手軽に（そこそこ安全に）できるようにしたい
- 関数が即座にbotとして公開されるというのは結構強めの機能

このあたりは結構うまく機能した

- 設定とコマンドラインオプション (flag) との使い分けの仕方
- 設定は環境変数経由で依存を含んだコンポーネントとして実体化
- コマンドラインオプションとはユーザー入力

ちょっとだけ脱線

- 集約queryみたいなものは提供したいかもしれない
- それを定期的に実行したかったりするんだろうか？


## 今年の目標？

このあたり悪くなさそう

- 好きなものを好きと言えるようになる
- 定期的に顔を出せそうなどこかに入る
- issueにyes/no以外の表現で答えられるようになりたい



