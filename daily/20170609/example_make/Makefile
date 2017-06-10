VALUE ?= :default:
export VALUE2 = ${VALUE}

foo:
	$(MAKE) -f foo.mk

foo2:
	VALUE=${VALUE} $(MAKE) -f foo.mk

foo3:
	VALUE=${VALUE} $(MAKE) -f foo.mk value2
