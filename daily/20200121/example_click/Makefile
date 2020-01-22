00:
	python $(shell echo $@*.py) --help 2>&1 | tee $(patsubst %.py,%.help,$(shell echo $@*.py))
01:
	python $(shell echo $@*.py) --help 2>&1 | tee $(patsubst %.py,%.help,$(shell echo $@*.py))
	python $(shell echo $@*.py) ec2 --help 2>&1 | tee $(patsubst %.py,%.help.ec2,$(shell echo $@*.py))
	python $(shell echo $@*.py) s3 --help 2>&1 | tee $(patsubst %.py,%.help.s3,$(shell echo $@*.py))

02:
	handofcats $(shell echo $@*.py) --help 2>&1 | tee $(patsubst %.py,%.help,$(shell echo $@*.py))
