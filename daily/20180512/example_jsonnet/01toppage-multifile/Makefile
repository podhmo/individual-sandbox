default:
	mkdir -p output
	jsonnet -m output example3.jsonnet

	# see: https://github.com/google/go-jsonnet/issues/118
	for i in `ls output/*`; do python -c 'import sys; import ast; print(ast.literal_eval(open(sys.argv[1]).read()))' $$i > tmp; mv tmp $$i; done

setup:
	go get -v github.com/google/go-jsonnet/jsonnet
	go get -v github.com/google/go-jsonnet/linter/jsonnet-lint
