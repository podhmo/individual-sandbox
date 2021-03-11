variable "aws_region" {
  default = "us-west-1a"
}

# todo template_file
locals {
  json = jsonencode(
    {
      name = "foo"
      age = 20
    }
  )
}

resource "local_file" "person_foo" {
  content = local.json
  filename = "foo.json"
}

output "json_result" {
  value = local.json
}
