00:
	cd $@*/ && invoke hello
	cd $@*/ && invoke bye --name bye!
	cd $@*/ && invoke --list || exit 0
	cd $@*/ && invoke bye -h

01:
	python multihand.py $@*.py hello
	python multihand.py $@*.py bye --name bye!
	python multihand.py $@*.py -h || exit 0
	python multihand.py $@*.py bye -h
