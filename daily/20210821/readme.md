## bigquery windows terraform

- [なるべくCLIでGKEクラスタを作成してみる - Qiita](https://qiita.com/ttr_tkmkb/items/26328fbbf5f17b920046)
- https://github.com/lukesampson/scoop-extras

```console
$ scoop install gcloud
$ gcloud init

$ gcloud config set compute/region asia-northeast
$ gcloud config set compute/zone asia-northeast1-a

$ gcloud projects list
```

## terraform

```console
$ scoop install terraform
$ gcloud auth application-default login
```

- https://registry.terraform.io/providers/hashicorp/google/latest/docs/guides/getting_started#configuring-the-provider


