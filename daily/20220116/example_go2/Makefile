GO ?= go1.18beta1

DIR = $(shell echo $@*/)
00:
	rm -rf $(DIR)sample.db
	cat $(DIR)insert.sql | sqlite3 $(DIR)sample.db
	DB=$(DIR)sample.db $(GO) run $(DIR)main.go

01:
	rm -rf $(DIR)sample.db
	cat $(DIR)insert.sql | sqlite3 $(DIR)sample.db
	DB=$(DIR)sample.db $(GO) run $(DIR)main.go


