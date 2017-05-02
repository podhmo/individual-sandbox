default:
	python cli.py  || echo "err"
	python cli.py -x 10 -y 3  || echo "err"
	python cli.py -y 0  || echo "err" # error
	python cli.py -y 0  --debug  || echo "err"  # error(verbose)
