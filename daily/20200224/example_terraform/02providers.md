# terraformの型定義とは？

- https://www.terraform.io/docs/configuration/resources.html
- https://www.terraform.io/docs/configuration/providers.html

どうもproviderがresourcesを所有するものっぽい感じらしいのだけれど。自分独自でこれらの設定を作ったりするのはcustom providerということっぽい？そしてgoを書く。

```
provider "google" {
  project = "acme-app"
  region  = "us-central1"
}
```

- https://qiita.com/keiichi-hikita/items/bb9b799c1e692f224d07
- https://febc-yamamoto.hatenablog.jp/entry/terraform-custom-provider-01
