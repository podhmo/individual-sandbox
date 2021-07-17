default:
	terraform init
	terraform validate
	terraform apply -auto-approve
	( cd sub && terraform apply -auto-approve )
