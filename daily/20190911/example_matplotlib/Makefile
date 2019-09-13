FILENAME ?= sample_plt.png
OPT ?= # --debug

default:
	@echo "---- info ------------------------------"
	$(MAKE) info
	@echo "----------------------------------------"
	$(MAKE) gen
	FILENAME=sample_plt.png $(MAKE) 00
	FILENAME=sample_plt_nbconvert.png $(MAKE) 02
	@echo "---- result ----------------------------"
	$(MAKE) check
	@echo "----------------------------------------"

check:
	identify *.png
gen:
	jupyter-nbconvert --to python save.ipynb --output save.py

info:
	@uname -a
	@python -V
	@pip freeze | grep -i notebook
	@pip freeze | grep -i matplotlib

00:
	python save.py
01:
	jupyter-notebook --ip=127.0.0.1
02:
	jupyter-nbconvert --execute --to notebook --allow-errors ${OPT} save.ipynb
