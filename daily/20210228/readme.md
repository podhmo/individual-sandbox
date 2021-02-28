## go mongodb sql

- https://docs.mongodb.com/manual/tutorial/query-documents/
- https://docs.mongodb.com/manual/reference/sql-comparison/
- https://docs.mongodb.com/manual/reference/operator/aggregation/lookup/#pipe._S_lookup


## terraform aws

自分でやってみる。これに従って作業するのが良さそう。

https://learn.hashicorp.com/collections/terraform/aws-get-started

1. install tfenv
1. install terraform
1. aws configure

main.tf


## tfenv

- https://github.com/tfutils/tfenv

```console
$ ghq get tfutils/tfenv
$ cp ~/qhq/github.com/tfutils/tfenv/bin/tfenv ~/bin
# 以下を ~/bin/tfenvの先頭に追加
# export TFENV_ROOT="$(echo ~/ghq/github.com/tfutils/tfenv)"

$ tfenv install latest
$ tfenv use latest
```

## terraform

localのdockerを管理したりもできるのか

- https://learn.hashicorp.com/tutorials/terraform/install-cli?in=terraform/aws-get-started
