root:
	terraform init
	terraform validate
	terraform apply -auto-approve

run-sub: root
	$(MAKE) -C subs
.PONY: run-sub

.DEFAULT_GOAL := run-sub