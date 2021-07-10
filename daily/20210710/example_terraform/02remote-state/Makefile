default:
	terraform init
	terraform apply -auto-approve
	cd sub && terraform init
	cd sub && terraform apply -auto-approve

clean:
	rm -f *.tfstate*
	rm -f sub/*.tfstate*