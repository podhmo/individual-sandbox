default: o e o2 e2 o3 e3

o:
	~/venvs/my/bin/python main.py
e:
	~/.emacs.d/.python-environments/default/bin/python main.py
o2: # ok
	VIRTUAL_ENV=~/venvs/my ~/venvs/my/bin/python main.py
e2: # ok
	VIRTUAL_ENV=~/venvs/my ~/.emacs.d/.python-environments/default/bin/python main.py
o3:
	VIRTUAL_ENV=~/.emacs.d/.python-environments/default ~/.emacs.d/.python-environments/default/bin/python main.py
e3:
	VIRTUAL_ENV=~/.emacs.d/.python-environments/default ~/.emacs.d/.python-environments/default/bin/python main.py
