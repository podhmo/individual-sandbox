default: o e o2 e2

o: # ok
	VIRTUAL_ENV=~/venvs/my ~/venvs/my/bin/python main.py
e: # ok
	VIRTUAL_ENV=~/venvs/my ~/.emacs.d/.python-environments/default/bin/python main.py
o2: # ok
	~/venvs/my/bin/python main.py --venv=~/venvs/my/
e2: # ok
	~/.emacs.d/.python-environments/default/bin/python main.py --venv=~/venvs/my/
