resource "local_file" "config" {
    count = var.savefile == "" ? 0 : 1
    filename = var.savefile
    file_permission = 0744
    content = jsonencode(var.config)
}
