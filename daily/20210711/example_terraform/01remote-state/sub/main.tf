data "terraform_remote_state" "root" {
    backend = "local"
    config = {
        path = "../terraform.tfstate"
    }
}


output "message" {
    value = "hello ${data.terraform_remote_state.root.outputs.config.person.name}!!"
}