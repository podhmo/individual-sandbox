terraform {
  backend "local" {
    path = "local.tfstate"
  }
}

data "external" "api-key" {
  program = ["echo", "{\"key\": \"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\"}"]
  query = {
    id = "xxx"
  }
}
