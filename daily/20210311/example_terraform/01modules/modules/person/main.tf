locals {
  json_body = jsonencode({
    name = var.name
    age = 20
  })
}
