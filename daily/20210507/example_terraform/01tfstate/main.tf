terraform {
  backend "local" {
    path = "local.tfstate"
  }
}

provider "random" {}

resource "random_password" "password" {
  length           = 16
  special          = true
  override_special = "_%@"
}
