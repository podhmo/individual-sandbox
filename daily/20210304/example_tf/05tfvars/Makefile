
default:
	FILE=develop.tfvars $(MAKE) show
	FILE=production.tfvars $(MAKE) show

show: FILE ?= develop.tfvars
show: .terraform.lock.hcl
show:
	terraform apply -var-file=$(FILE) -auto-approve

.terraform.lock.hcl:
	terraform init
.PHONY: init

fmt:
	terraform fmt
.PHONY: fmt
