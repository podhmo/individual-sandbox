output "result" {
  value = {
    s3_bucket = {
      bucket = "${var.prefix}-app-backup"
      acl    = "private"
    }
  }
}
