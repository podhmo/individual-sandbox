define gorunT =
	mkdir -p public
	cp $(1) public/`echo $(1) | sed 's@/@_@g'`
	go run $(1) | tee $(addsuffix .output.txt,$(basename $(1)))

endef

WS := %

# help
help:
	@cat Makefile | grep --color=auto -B 1 '^[^:][^ :]*:' | grep -v "^--$$" -- | tr "\n" "$(WS)" | sed 's/$(WS)#/ --/g' | tr "$(WS)" "\n" | sort

# default action
default:
	$(MAKE) -n rungo-static
	$(MAKE) -n rungo-dynamic
	$(MAKE) -n rungo-dynamic-shell

files := abc/main.go xyz/main.go

# go run */main.go (but reading statically defined variable)
rungo-static:
	echo $(files)
	$(foreach gomain,$(files),$(call gorunT,$(gomain)))

# go run */main.go (find main.go via make's function)
rungo-dynamic:
	$(foreach gomain,$(wildcard */main.go),$(call gorunT,$(gomain)))

# go run */main.go (find main.go via shell)
rungo-dynamic-shell:
	$(foreach gomain,$(shell find . -name "main.go" | sed 's@^./@@g'),$(call gorunT,$(gomain)))

# pythontest:
# 	echo $(dir $(wildcard */setup.py))
