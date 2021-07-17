locals {
    person = {
        name = "foo"
        age = 20
    }
}

output "config" {
    value = {
        person = local.person
    }
}