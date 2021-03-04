variable "foo" {
  type = string
  default = "FOO"
}

locals {
  name = "yyy"
  common_tags = {
    Foo = var.foo
    Owner   = local.name
  }
}

resource "null_resource" "hello" {
  triggers = {
    xxx = "hmmmmmmm"
    yyy = "hmm:${data.external.example.result.message}"
    n = "hmm:${data.external.example.result.n}"
  }
  provisioner "local-exec" {
    interpreter = ["bash", "-ux", "-c"]
    command = <<EOS
echo hello '${var.foo}'
echo triggers.yyy is ${null_resource.hello.triggers.yyy}
echo n = ${data.external.example.result.n}
EOS
  }
}

resource "null_resource" "web" {
  # ...

  provisioner "local-exec" {
    command = "echo $FOO $BAR $BAZ >> env_vars.txt"

    environment = {
      FOO = "bar"
      BAR = 1
      BAZ = "true"
    }
  }
}

data "external" "example" {
#  program = ["python", "-c", "'import sys; import json; print(json.dumps(dict(value=1111, args=sys.argv)))'"]
  program = ["python", "x.py"]
  query = {
    # arbitrary map from strings to strings, passed
    # to the external program as the data query.
    id = "abc123"
  }
}
