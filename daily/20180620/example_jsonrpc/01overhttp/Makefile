default:
	rm -rf dist
	mkdir -p dist
	for i in `find . -name "*.go"`; do cp $$i dist/`echo $$i | sed 's@^\./@@g;s@/@_@g'`; done
