default:
	python -m testmarker discover tests --verbose

00:
	NO_A=1 python -m unittest discover tests --verbose

01:
	NO_A=1 NO_B=1 python -m unittest discover tests --verbose
02:
	python -m unittest discover tests2 --verbose
03:
	X=1 python -m unittest discover tests2 --verbose

04:
	python -m testmarker discover tests --verbose --ignore=a,b
05:
	python -m testmarker discover tests --verbose --only=a
