python-packages := $(shell find . -name "setup.py" | xargs dirname | sed 's@^\./@@g; s@/$$@@g')
TARGET ?= 

define defTestTaskT =
test$(basename $(1)):
	python $(1)/setup.py -q test

endef

default:
	$(MAKE) test${TARGET}

$(eval test: $(addprefix test,$(basename $(python-packages))))
$(foreach p,$(python-packages),$(eval $(call defTestTaskT,$(p))))

