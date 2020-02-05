CLOUDSDK_PYTHON=$(shell which python)

install:
	CLOUDSDK_PYTHON=$(shell which python) ./google-cloud-sdk/install.sh
	for i in $$(ls ./google-cloud-sdk/bin); do ln -f -s $(shell pwd)/google-cloud-sdk/bin/$$i bin/$$i; done
