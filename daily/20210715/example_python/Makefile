00:
	docker build -t foo:0.0.0 .
01:
	DOCKER_BUILDKIT=1 docker build -t foo:0.1.0 . -f Dockerfile.cached --no-cache
02:
	DOCKER_BUILDKIT=1 docker build -t foo:0.2.0 . -f Dockerfile.nocache
03:
	DOCKER_BUILDKIT=1 docker build -t foo:0.3.0 . -f Dockerfile.cache-from --progress=plain --cache-from foo:0.0.0 --no-cache
