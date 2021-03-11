variable "aws_region" {
  default = "us-west-1a"
}

locals {
  json = jsonencode(
    {
      name = "foo"
      age = 20
    }
  )
}

output "json_result" {
  value = local.json
}
