DENO ?= $(shell echo $(HOME)/.deno/bin/deno)

00:
	$(DENO) --allow-net $@*.ts
01:
	$(DENO) --allow-net $@*.ts
02:
	$(DENO) --allow-net $@*.ts
03:
	$(DENO) --allow-net https://servestjs.org/example/use_jsx.tsx

setup:
	curl -fsSL https://deno.land/x/install/install.sh | bash -x -u -s v0.2.10
