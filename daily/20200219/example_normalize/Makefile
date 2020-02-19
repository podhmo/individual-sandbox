00:
	cd ../../ && git ls-files
01:
	# https://brendanforster.com/notes/fixing-invalid-git-paths-on-windows/
	( cd ../../ && git ls-files ) | python rep.py | tee migrate.sh
02:
	cd ../../ && bash -u -x $(shell pwd)/migrate.sh
