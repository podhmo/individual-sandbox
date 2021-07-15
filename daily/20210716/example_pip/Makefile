bin/proxy: *.go
	mkdir -p bin
	go build -o $@ $^

.venv/bin/python:
	python -m venv .venv

PROXY := --proxy=http://localhost:33333 --trusted-host pypi.org --trusted-host files.pythonhosted.org
# PROXY :=
setup: .venv/bin/python
	.venv/bin/python -m pip install $(PROXY) --only-binary=:all: --no-cache-dir --force-reinstall -r requirements.txt

build-image:
	DOCKER_BUILDKIT=1 docker --debug --config docker-config build -f Dockerfile.arm -t bar:0.0.0 --progress plain --no-cache . --build-arg PIP_OPTION='--trusted-host pypi.org --trusted-host files.pythonhosted.org'
run-image:
	docker --config docker-config run -it --rm bar:0.0.0 bash

ping:
	ALL_PROXY=http://localhost:33333 curl http://httpbin.org/ip

# run-chocon:
# 	bin/chocon -p 8888 --upstream http://localhost:33333
# .PHONY: run-chocon

run-proxy: bin/proxy
	# VERBOSE=1 PORT=33333 bin/proxy
	PORT=33333 bin/proxy
.PHONY: run-proxy
