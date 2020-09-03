## terraform

久しぶりで忘れた。この辺みるのが良い。

- https://reboooot.net/post/what-is-terraform/
- https://future-architect.github.io/articles/20190816/

個人として使うなら以下を覚えれば良さそう。

```console
$ tfenv xxx
$ terraform init
$ terraform plan
$ terraform apply
```

`-target`で対象を絞れる。

```console
$ terraform apply -target=<target name>
```


### hello world?

s3などにstateを保存しておくことがお勧めされている。

- https://learn.hashicorp.com/tutorials/terraform/aws-remote
