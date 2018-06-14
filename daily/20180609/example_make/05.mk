SUBDIRS := $(dir $(shell find . -name "readme.md"))

.%.title: %.md
	cat $< | grep -P '^#[^#]' | tee $@

default: $(addsuffix .readme.title,${SUBDIRS})
	cat $^

clean:
	find . -name ".readme.title" | xargs rm -vf
.PHONY: clean
