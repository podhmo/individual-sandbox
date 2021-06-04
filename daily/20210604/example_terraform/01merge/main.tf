module "a" {
  source = "./modules/xy"
}

module "b" {
  source = "./modules/xy"
  environments = {
    token = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
  }
}
