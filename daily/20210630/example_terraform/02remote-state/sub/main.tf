data "terraform_remote_state" "root" {
    backend = "local" 
    config = {
        path = "../xxx.terraform.tfstate"
    }
}

output "message" {
    value = "hello world"
}

output "message2" {
    value = "${data.terraform_remote_state.root.outputs.person.name}: hello world"
}