SHELL := bash

FOO ?= foo
export TF_VAR_foo=$(FOO)

00:
	cd $(shell echo $@*/)&& terraform init && terraform apply -auto-approve
	cat $(shell echo $@*/)local.tfstate
01:
	cd $(shell echo $@*/)&& terraform init && terraform apply -auto-approve
	cat $(shell echo $@*/)local.tfstate
02:
	cd $(shell echo $@*/)&& terraform init && terraform apply -auto-approve
	cat $(shell echo $@*/)local.tfstate
03:
	cd $(shell echo $@*/)&& terraform init && terraform apply -auto-approve
