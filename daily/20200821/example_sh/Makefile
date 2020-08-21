SHELL := $(shell which bash)

00: run = bash $(shell echo $@*.sh)
00:
	$(run)
	FOO=foo $(run)
	source .env && $(run)
	source <( sed 's/^/export /g' .env ) && $(run)
	bash <(cat .env $(shell echo $@*.sh) )
