locals {
  x = 10
  y = 20
  z = 30
}

resource "null_resource" "echo" {
  triggers = {
    x = local.x
    y = local.y
    z = local.z
  }

  provisioner "local-exec" {
    command = <<EOS
echo x=${local.x} y=${local.y} z=${local.z}
EOS
  }
}
