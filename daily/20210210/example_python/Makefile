TARGET_DIR = $(shell echo $@*/)

00:
	swagger-marshmallow-codegen --strict $(TARGET_DIR)openapi.json | tee $(TARGET_DIR)schema.py
	python $(TARGET_DIR)main.py
01:
	swagger-marshmallow-codegen --strict $(TARGET_DIR)openapi.json | tee $(TARGET_DIR)schema.py


