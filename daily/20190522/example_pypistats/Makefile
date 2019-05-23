parse2: autopep8.json yapf.json black.json
	echo '["autopep8.json", "yapf.json", "black.json"]' | jqfpy 'from functools import reduce; data = get(); reduce(lambda acc, fname: h.merge(acc, h.loadfile(fname)["data"], prefix=f"{fname[:-5]}_"), data, [])' --additionals merge.py | dictknife cat -i json -o md

parse: autopep8.json yapf.json black.json
	echo '{"left": "autopep8.json", "right": "yapf.json"}' | jqfpy 'd = get(); h.merge(h.loadfile(d["left"])["data"], h.loadfile(d["right"])["data"])' --additionals merge.py
	echo '{"left": "autopep8.json", "right": "black.json"}' | jqfpy 'd = get(); h.merge(h.loadfile(d["left"])["data"], h.loadfile(d["right"])["data"])' --additionals merge.py

black.json:
	pypistats python_minor black -f json | tee $@
autopep8.json:
	pypistats python_minor autopep8 -f json | tee $@
yapf.json:
	pypistats python_minor yapf -f json | tee $@
