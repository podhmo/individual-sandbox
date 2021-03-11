variable "aws_region" {
  default = "us-west-1a"
}

module "foo" {
  source = "./modules/person"
  name   = "foo"
}

module "bar" {
  source = "./modules/person"
  name   = "bar"
}

output "foo_result" {
  value = module.foo.json_body
}
output "bar_result" {
  value = module.bar.json_body
}
