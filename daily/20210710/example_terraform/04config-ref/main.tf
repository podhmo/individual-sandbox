locals  {
    foo_path = abspath("${path.module}/data/foo.json")
}
module "foo_config" {
    source = "./modules/person/modules/store"
    filename = local.foo_path
    config = {
        name = "foo"
        age = 20
    }
}
output "person_path" {
    value = local.foo_path
}