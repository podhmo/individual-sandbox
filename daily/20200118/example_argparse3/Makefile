gen: targets
	for i in `seq 10`; do echo $$i > targets/`printf "%02dtarget.txt" $$i`; done

targets:
	mkdir -p $@

00:
	ls targets/*.txt | xargs -n 1 python $(shell echo $@*.py) foo
01:
	python $(shell echo $@*.py) foo targets/*.txt
02:
	python $(shell echo $@*.py) \
 foo targets/target01.txt - bar targets/target02.txt - \
 foo targets/target03.txt - bar targets/target04.txt - \
 foo targets/target05.txt - bar targets/target06.txt - \
 foo targets/target07.txt - bar targets/target08.txt - \
 foo targets/target09.txt - bar targets/target10.txt
