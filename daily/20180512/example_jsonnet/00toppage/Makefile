define genT =
$(1):
	@mkdir -p output
	@echo == input $(1) == > /dev/stderr
	@cat $(1) > /dev/stderr
	@echo > /dev/stderr
	@echo == output $(1) == > /dev/stderr

	jsonnet $(1) | tee output/$(shell echo $(1) | sed 's/\.jsonnet$$/.json/')
	@echo > /dev/stderr
	@echo > /dev/stderr

.PHONY: $(1)

endef

# generate tasks
$(eval default: $(wildcard *.jsonnet))
$(foreach src,$(wildcard *.jsonnet),$(eval $(call genT,$(src))))

clean:
	rm -rf output
.PHONY: clean
