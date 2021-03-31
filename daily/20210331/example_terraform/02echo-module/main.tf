module "echo1" {
  source = "./modules/xyz"

  primary_configuration = {
    x = 10
    y = 20
    z = 30
  }
}

module "echo2" {
  source = "./modules/xyz"

  primary_configuration = {
    x = 100
    y = 200
    z = 300
  }
}
