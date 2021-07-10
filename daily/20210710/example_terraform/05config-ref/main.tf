module "foo_config" {
    source = "./modules/person"
    savefile = "${path.module}/data/foo.json"
    config = {
        name = "foo"
        age = 20
    }
}
