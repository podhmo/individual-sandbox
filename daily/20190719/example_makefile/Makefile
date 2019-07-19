-include ./00.mk
-include ./01.mk
-include ./02.mk

default:
	$(MAKE) 00 |& grep -vP '^make\[\d' | tee 00.output
	$(MAKE) 01 |& grep -vP '^make\[\d' | tee 01.output
	$(MAKE) 02 |& grep -vP '^make\[\d' | tee 02.output
.DEFAULT_GOAL = default
