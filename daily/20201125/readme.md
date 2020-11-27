## terraform lsp

- https://github.com/hashicorp/terraform-ls

## aws関係 tui

やっぱりaccordionのようなものが欲しい。

- bulletとかは近い。[../20200921/example_pycli/50bullet_check.py](../20200921/example_pycli/50bullet_check.py)

  - ただmulti lineのoutputができないのでむりかも

- bubbleteaとかは表示できたかも [../20201021/example_go]()
- 無理なら https://github.com/prompt-toolkit/pymux を参考に２画面にする？
- あるいは本当に２画面立ち上げて通信させる



## aws lambdaの使えるruntimeは？

- https://stackoverflow.com/questions/53496412/aws-lambda-to-list-all-available-language-runtimes
- https://github.com/boto/botocore/blob/develop/botocore/data/lambda/2015-03-31/service-2.json#L4107-L4132

```
    "Runtime":{
      "type":"string",
      "enum":[
        "nodejs",
        "nodejs4.3",
        "nodejs6.10",
        "nodejs8.10",
        "nodejs10.x",
        "nodejs12.x",
        "java8",
        "java8.al2",
        "java11",
        "python2.7",
        "python3.6",
        "python3.7",
        "python3.8",
        "dotnetcore1.0",
        "dotnetcore2.0",
        "dotnetcore2.1",
        "dotnetcore3.1",
        "nodejs4.3-edge",
        "go1.x",
        "ruby2.5",
        "ruby2.7",
        "provided",
        "provided.al2"
```

## aws lambda ちょっと手元で動かそうとしてみる

- https://docs.aws.amazon.com/ja_jp/lambda/latest/dg/gettingstarted-awscli.html

手順はなんだろう？

1. 実行ロールの作成
2. lambda 関数の作成
3. (list lambda関数)
4. 削除

ログを取り出す部分とかがだいぶ怠いね。。

### lambroll

https://github.com/fujiwara/lambroll

コレで動かそうとしてみる。

```console
$ go get -v github.com/fujiwara/lambroll/cmd/lambroll
```

なるほどできた。

### 外へのアクセス

- https://aws.amazon.com/jp/premiumsupport/knowledge-center/internet-access-lambda-function/

そういえば、VPCの権限が必要だっけ？
何かを試してみると良さそうだけれど。

## aws create-roleの権限がない

```
aws iam create-role --role-name lambda-ex --assume-role-policy-document file://trust-policy.json

An error occurred (AccessDenied) when calling the CreateRole operation: User: arn:aws:iam::784330574880:user/monogusa is not authorized to perform: iam:CreateRole on resource: arn:aws:iam::784330574880:role/lambda-ex
make: *** [00] Error 254
```

一時的に別の形で認証ってできなかったっけ？

- https://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_temp_use-resources.html

```console
export AWS_ACCESS_KEY_ID=AKIAI44QH8DHBEXAMPLE
export AWS_SECRET_ACCESS_KEY=wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY
export AWS_SESSION_TOKEN=AQoDYXdzEJr...<remainder of security token>
aws ec2 describe-instances --region us-west-1
```

こんなかんじか

- https://dev.classmethod.jp/articles/sts-temporality-credential/

```
$ aws sts assume-role \
  --role-arn          arn:aws:iam::xxxxxxxxxxxx:role/xxxxxxxxxxxx \
  --role-session-name foo-bar-session \
  --duration-second   900 \
  --profile           foo-bar \
  --serial-number     arn:aws:iam::xxxxxxxxxxxx:mfa/xxxxxxxxxxxx \ # MFAデバイスのARN
  --token-code        123456 
```

まぁあとで

## go typed text/template

## go asttemplate

- astmpl
- ast as template

https://github.com/cheekybits/genny との違いは？

シンボル名を変えたい。

