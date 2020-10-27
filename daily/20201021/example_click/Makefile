cmd ?= foo_bar

00:
	python $(cmd) --help | tee help.txt
	python $(cmd) envvar --help | tee help.envvar.txt
	python $(cmd) color --help | tee help.color.txt

01:
	_FOO_BAR_COMPLETE=source_bash python $(cmd) | tee completion.bash
	_FOO_BAR_COMPLETE=source_zsh python $(cmd) | tee completion.zsh
	_FOO_BAR_COMPLETE=source_fish python $(cmd) | tee completion.fish

10:
	python $(shell echo $@*.py)
