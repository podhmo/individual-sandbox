terraform {
  backend "local" {
    path = "local.tfstate"
  }
}

locals {
  xs = [1, 2, 3]
}

output "xs" {
  value = concat(local.xs, [10])
}
