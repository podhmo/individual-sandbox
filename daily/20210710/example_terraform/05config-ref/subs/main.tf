module "person" {
    source = "../modules/person"
    config = jsondecode(file("../data/foo.json"))
}


output "message" {
    value = "hello ${module.person.config.namee}"
}