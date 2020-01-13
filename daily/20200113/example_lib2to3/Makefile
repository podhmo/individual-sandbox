

00:
	python $(shell echo $@*.py)
01: TARGET ?= 00walk.py
01:
	python -m prestring.python $(TARGET)

dump: TARGET ?= hello.py
dump:
	python -m libcst.tool print $(TARGET) > $(patsubst %.py,%.libcst,$(TARGET))
	python dump.py $(TARGET) > $(patsubst %.py,%.lit2to3,$(TARGET))
