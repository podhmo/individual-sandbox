variable "primary_configuration" {
  type = object({
    x = number
    y = number
    z = number
  })
  default = {
    x = 10
    y = 20
    z = 30
  }
}

module "plain" {
  source = "./modules/xyz"

  primary_configuration = var.primary_configuration
}
