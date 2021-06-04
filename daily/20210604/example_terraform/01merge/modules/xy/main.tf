locals {
  body = jsonencode(
    {
      container_definitions = [
        {
          "environments": concat([
            {"name": "CONTROLLED_BY", "value": "terraform"}
          ], [for k, v in var.environments : {"name": k, "value": v}])
        }
      ]
    }
    )
}

resource "null_resource" "cluster" {
  triggers = {
    body = "a${local.body}"
  }

  provisioner "local-exec" {
    command = "echo body: \"${local.body}\""
  }
}
