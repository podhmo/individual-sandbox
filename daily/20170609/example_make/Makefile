VALUE ?= :default:
export VALUE2 = ${VALUE}
export FOO = foo

foo:
	$(MAKE) -f foo.mk

foo2:
	VALUE=${VALUE} $(MAKE) -f foo.mk

foo3:
	VALUE=${VALUE} $(MAKE) -f foo.mk value2

default:
	$(MAKE) -f foo.mk
