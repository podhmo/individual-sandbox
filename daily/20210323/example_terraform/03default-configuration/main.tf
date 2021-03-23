locals {
  primary_configuration = {
    x = 10
    y = 20
    z = 30
  }
}

module "plain" {
  source = "./modules/xyz"

  primary_configuration = local.primary_configuration
}
