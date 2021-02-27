# export TF_LOG := debug
# export TF_LOG := trace
export AWS_PROFILE := terraform

00:
	terraform init

01:
	terraform fmt

02:
	terraform apply

## upload fake
10:
	aws s3 ls s3://podhmo-my-tf-test-bucket/
11:
	aws s3 cp s3.tf s3://podhmo-my-tf-test-bucket/

clean:
	aws s3 rm s3://podhmo-my-tf-test-bucket/s3.tf
	terraform destroy
.PHONY: clean
