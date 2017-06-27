OPTS = --traceback

default: hello.html hello2.html

hello.html: hello.rst
	rst2html5.py ${OPTS} $< > $@

hello2.html: hello2.rst
	python myrst2html5.py ${OPTS} $< > $@

clean:
	rm -f *.html
