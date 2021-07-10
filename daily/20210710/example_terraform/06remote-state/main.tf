module "foo_config" {
    source = "./modules/person"
    savefile = "${path.module}/data/foo.json"
    config = {
        name = "foo"
        age = 20
    }
}

output "foo_config" {
    value = module.foo_config.config
}