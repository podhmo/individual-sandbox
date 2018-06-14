SUBDIRS := $(dir $(shell find . -name "readme.md"))

.%.title: %.md
	cat $< | grep -P '^#[^#]' | tee $@

# 全ての依存を使うタスクを定義
default: $(addsuffix .readme.title,${SUBDIRS})
	cat $^

# 依存関係を定義
define genTitleT
$(1).readme.title: $(1)readme.md

endef

$(foreach d,${SUBDIRS},$(eval $(call genTitleT,$(d))))

clean:
	find . -name ".readme.title" | xargs rm -vf
.PHONY: clean
