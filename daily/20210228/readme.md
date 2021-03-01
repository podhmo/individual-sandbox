## hmm

https://www.reddit.com/r/golang/comments/lpso9c/can_you_recommend_open_source_projects_with_small/

- https://github.com/mdlayher/arp
- https://github.com/dop251/goja
- https://github.com/preslavmihaylov/todocheck

## go migrate db

- https://dev.to/techschoolguru/how-to-write-run-database-migration-in-golang-5h6g
- https://tableplus.com/

んー。

- https://techinscribed.com/create-db-migrations-tool-in-go-from-scratch/
- https://github.com/praveen001/go-db-migration

## go mongodb sql

- https://docs.mongodb.com/manual/tutorial/query-documents/
- https://docs.mongodb.com/manual/reference/sql-comparison/
- https://docs.mongodb.com/manual/reference/operator/aggregation/lookup/#pipe._S_lookup


その前にsqlxの復習が必要そうだ。。

- create table
- insert
- parameterized query
- (partial application)
- pagination

### pagination

https://ivopereira.net/efficient-pagination-dont-use-offset-limit

これでsortされる場合ってどうなるんだろう？ `order by (xxx, id)` でいける？

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
