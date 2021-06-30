locals {
    foo = {
        name = "foo"
        age = 20
    }
}

output "person" {
    value = local.foo
}