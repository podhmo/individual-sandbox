## python new style schema library

- 循環参照を記述するための `from __future__ import annotations`
- `__set_name__` を使ってpropertyの名前を保持
- `__init_subclass__` を使って整合性チェック

## terraformをpythonで模倣

困るところはどのあたりか

- クラス名
- resourceごとにnamespaceを持っている
- (dataclassesだとrequired,unrequiredの対応がめんどくさい)

そもそもterraformを意識する必要はあるんだろうか？
モジュールが別れていれば良いのでは？

```
from aws import aws_instance

# ここでexampleが他に使えないのが辛い
example = aws_instance(ami="ami-0c55b159cbfafe1f0", instance_type="t2.micro")

# 参照的にはaws_instance.exampleができれば良い
r.aws_instance.example = aws_instance(ami="ami-0c55b159cbfafe1f0", instance_type="t2.micro")

# arnを取り出せる
r.aws_instance.arn
```


## terraformの良さ

arnを意識しなくて済む

```
resource "aws_instance" "example" {
  ami           = "ami-0c55b159cbfafe1f0"
  instance_type = "t2.micro"
}
```

逆に言うとarnを入力に取るようなツールは単体ではまともに使えない。

```
{
  "version": 4,
  "terraform_version": "0.12.0",
  "serial": 1,
  "lineage": "1f2087f9-4b3c-1b66-65db-8b78faafc6fb",
  "outputs": {},
  "resources": [
    {
      "mode": "managed",
      "type": "aws_instance",
      "name": "example",
      "provider": "provider.aws",
      "instances": [
        {
          "schema_version": 1,
          "attributes": {
            "ami": "ami-0c55b159cbfafe1f0",
            "availability_zone": "us-east-2c",
            "id": "i-00d689a0acc43af0f",
            "instance_state": "running",
            "instance_type": "t2.micro",
            "(...)": "(truncated)"
          }
        }
      ]
    }
  ]
}
```
