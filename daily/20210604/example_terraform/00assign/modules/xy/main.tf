resource "null_resource" "cluster" {
  # Changes to any instance of the cluster requires re-provisioning
  triggers = {
    xy = "${var.p.x}@${var.p.y}"
  }

  provisioner "local-exec" {
    command = "echo ${var.p.x}@${var.p.y}"
  }
}
