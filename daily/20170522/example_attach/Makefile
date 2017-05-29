run:
	# @python 00pdb.py > loop.pid
	@python 01traceback.py > loop.pid

attach:
	kill -s USR1 `cat loop.pid`
