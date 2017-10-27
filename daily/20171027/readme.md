## aws awscliでroleを使う方法

決まっているなら単にcredentialsに書けば良い

```
[default]
role_arn = arn:aws:iam::<id>:role/<rolename>
source_profile = user
region = us-east-1

[user]
aws_access_key_id = XXXXXXXXXxXxXXXXXXXX
aws_secret_access_key = xxxxXxxXxxXxxxxxXXxxxxXxxXXxXxXxxXxXxXXx
```

環境変数経由で渡す時、access_keyなどとは異なり環境変数で直接渡せない。

```
$ credentials=$(aws sts assume-role --role-arn <role> --role-session-name <name>)
# 以下の様な感じで設定していく
$ AWS_ACCESS_KEY_ID=echo $credentials | jq .Credentials.AccessKeyId
$ AWS_ACCESS_KEY_ID=<> AWS_SECRET_ACCESSS_KEY=<> AWS_SESSION_TOKEN=<> aws <cmd> <subcommand>
```

雑なpythonだとこんな感じ

```
$ eval $(aws sts assume-role --role-session-name=x --role-arn <role-arn> | python -c 'import sys; import json; d = json.load(sys.stdin)["Credentials"]; print("AWS_ACCESS_KEY_ID={} AWS_SECRET_ACCESS_KEY={} AWS_SESSION_TOKEN={}".format(d["AccessKeyId"], d["SecretAccessKey"], d["SessionToken"]))')
```


## docker command

|name|description|
|:--|:--|
|docker pull|imageの取得|
|docker login|registry serverにログイン|
|docker exec|dockerコマンド実行|

### ecr上のimageをpullしたい場合

`docker login` ってログイン情報どこに持つんだろ？

```
eval $(AWS_DEFAULT_REGION=us-east-1 aws ecr get-login --registry-ids <registry id>)
docker pull <registyr id>.dkr.ecr.us-east-1.amazonaws.com/ai-analyst-back:<image>
```


### dockerでbashを起動

そういえば、たいていの場合にはdocker-compose経由で使っていたのでdockerの細かなコマンドのことを覚えていない。

```
$ docker exec --it <image name> bash
```

