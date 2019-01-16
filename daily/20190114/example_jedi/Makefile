PYTHON ?= python

run:
	@echo "** which python >>" ${PYTHON} "**"
	@echo "** arrow installed" "**"
	@(${PYTHON} -c 'import arrow' &> /dev/null; if [ $$? -eq 0 ]; then echo ok; else echo ng; fi)
	@echo "** pendulum installed" "**"
	@(${PYTHON} -c 'import pendulum' &> /dev/null; if [ $$? -eq 0 ]; then echo ok; else echo ng; fi)


runA:
	$(MAKE) run PYTHON=a/bin/python
runB:
	$(MAKE) run PYTHON=b/bin/python

PYTHON ?= python
VENVPATH ?=

comp:
	@echo "** which python >>" ${PYTHON} "**"
	@echo "** venv path is >>" ${VENVPATH} "**"
	@echo "** arrow.no -> arrow.now?" "**"
	${PYTHON} 02usejedi.py arrow ${VENVPATH}
	@echo "** pendulum.no -> pendulum.now?" "**"
	${PYTHON} 02usejedi.py pendulum ${VENVPATH}

compAforA:
	$(MAKE) comp PYTHON=a/bin/python VENVPATH=a
compAforB:
	$(MAKE) comp PYTHON=a/bin/python VENVPATH=b

clean:
	rm -rf a b

setup:
	# 環境の作成
	python -m venv a
	python -m venv b
	# パッケージのインストール
	a/bin/pip install arrow jedi
	b/bin/pip install pendulum
