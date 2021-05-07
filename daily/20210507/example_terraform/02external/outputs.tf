output "message" {
  value = "key is ${data.external.api-key.result.key}"
}
