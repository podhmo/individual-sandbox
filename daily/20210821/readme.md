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


### 追記

こういうのが出てめんどくさい。

```
google_bigquery_table.default: Modifying... [id=projects/speedy-equator-284105/datasets/foo/tables/bar]
╷
│ Error: googleapi: Error 403: Billing has not been enabled for this project. Enable billing at https://console.cloud.google.com/billing. Table expiration time must be less than 60 days while in sandbox mode., billingNotEnabled
│
│   with google_bigquery_table.default,
│   on bigquery.tf line 14, in resource "google_bigquery_table" "default":
│   14: resource "google_bigquery_table" "default" {
```