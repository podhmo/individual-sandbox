default: 04

04:
	PYTHONASYNCIODEBUG=1 python -W default 04*.py |& tee /tmp/xxx 
parse:
	@echo put `grep -P put /tmp/xxx | grep -oP "id=[0-9]+" | sort| wc -l`
	@echo get `grep -P get /tmp/xxx | grep -oP "id=[0-9]+" | sort| wc -l`
	@echo done `grep -P done /tmp/xxx | grep -oP "id=[0-9]+" | sort| wc -l`
	@echo finish `grep -P finish /tmp/xxx | grep -oP "id=[0-9]+" | sort| wc -l`
	@echo continue `grep -P continue /tmp/xxx | grep -oP "id=[0-9]+" | sort| wc -l`
	@echo canceled `grep -P canceled /tmp/xxx | grep -oP "id=[0-9]+" | sort| wc -l`
	@echo error `grep -P error /tmp/xxx | grep -oP "id=[0-9]+" | sort| wc -l`
	@echo callback `grep -P callback /tmp/xxx | grep -oP "callback" | sort| wc -l`
