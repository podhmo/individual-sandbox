resource "null_resource" "echo" {
  triggers = {
    x = var.primary_configuration.x
    y = var.primary_configuration.y
    z = var.primary_configuration.z
  }

  provisioner "local-exec" {
    command = <<EOS
echo x=${var.primary_configuration.x} y=${var.primary_configuration.y} z=${var.primary_configuration.z}
EOS
  }
}
