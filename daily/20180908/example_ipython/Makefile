setup:
	pip install -r requirements.txt
02:
	jupyter-console
03:
	nbreversible 03nbreversible.py > 03nbreversible.ipynb
	jupyter-nbconvert 03nbreversible.ipynb --stdout --to=markdown --execute 2>&1 | tee 03nbreversible.txt
