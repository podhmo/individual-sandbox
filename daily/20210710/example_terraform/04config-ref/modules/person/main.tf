locals {
    config = jsondecode(file(var.filename))
}