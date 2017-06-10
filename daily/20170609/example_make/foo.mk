value:
	@echo 'this is input message "${VALUE}".'

value2:
	@echo 'this is input message "${VALUE2}".'

bar:
	make -f bar.mk
