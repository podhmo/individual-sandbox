default:
	for i in 0*.py; do python $$i 2>&1 > `echo $$i | sed 's/\.py$$/.output/'`; done
