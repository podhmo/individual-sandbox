TARGET ?= ../../../../pygal/docs/documentation

parse:
	find $(TARGET) -name "*.rst" | xargs cat | python parse.py
	$(MAKE) -j -C output
