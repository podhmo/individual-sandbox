locals {
  ob = {
    x = 10
    y = 20
    z = 30
  }
}

module "xy" {
  source = "./modules/xy"
  p      = local.ob
}
