output "message" {
  value = "hello ${var.foo}"
  sensitive = true
}
