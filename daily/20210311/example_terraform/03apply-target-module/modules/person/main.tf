locals {
  json_body = jsonencode({
    name = var.name
    age = 20
  })
}

resource "null_resource" "echo" {
 # Changes to any instance of the cluster requires re-provisioning
  triggers = {
   name = var.name
  }
  provisioner "local-exec" {
    command = <<EOS
echo ----------------------------------------
echo hello ${var.name}
echo ${local.json_body} | jq .
echo ----------------------------------------
EOS
  }
}
