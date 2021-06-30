locals {
    subject = "world"
}

output "result" {
    value = "hello ${local.subject}"
}