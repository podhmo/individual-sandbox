resource "local_file" "config" {
    filename = var.filename
    file_permission = 0744
    content = jsonencode(var.config)
}
