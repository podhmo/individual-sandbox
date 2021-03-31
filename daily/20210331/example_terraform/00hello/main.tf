variable "aws_region" {
  default = "us-west-1a"
}

locals {
  person = {
    name = "foo"
    age  = 20
  }
}

output "json_result" {
  value    = jsonencode(local.person)
}


output "greeting" {
  value = "hello ${local.person.name} from ${var.aws_region}"
}


