default: 00 01 02 03 04 05

FLAKE8 := flake8

# from handofcats import as_subcommand
00:
	python $(shell echo $@*/)cli.py --expose | tee $(shell echo $@*/)_expose.py
	$(FLAKE8) $(shell echo $@*)/_exposed.py

# import handofcats
01:
	python $(shell echo $@*/)cli.py --expose | tee $(shell echo $@*/)_expose.py
	$(FLAKE8) $(shell echo $@*)/_exposed.py

# from handofcats import as_subcommand as register
02:
	python $(shell echo $@*/)cli.py --expose | tee $(shell echo $@*/)_expose.py
	$(FLAKE8) $(shell echo $@*)/_exposed.py

# import handfcats as h
03:
	python $(shell echo $@*/)cli.py --expose | tee $(shell echo $@*/)_expose.py
	$(FLAKE8) $(shell echo $@*)/_exposed.py

# if name main
04:
	python $(shell echo $@*/)cli.py --expose | tee $(shell echo $@*/)_expose.py
	$(FLAKE8) $(shell echo $@*)/_exposed.py

# from handfcats import (x, y, z)
05:
	python $(shell echo $@*/)cli.py --expose | tee $(shell echo $@*/)_expose.py
	$(FLAKE8) $(shell echo $@*)/_exposed.py

