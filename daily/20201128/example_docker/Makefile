00:
	docker otbuild -t xxx:xxx .
01:
	docker run -it --rm -t xxx:xxx pwd
	docker run -it --rm -w /code/foo -t xxx:xxx pwd
02:
	# WORKDIRは/codeなのでhello.txtはない
	docker run -it --rm -t xxx:xxx cat hello.txt || :
	# WORKDIRは/codeなので./foo/hello.txtなら大丈夫
	docker run -it --rm -t xxx:xxx cat ./foo/hello.txt || :
	# WORKDIRは/code/fooなのでhello.txtでも大丈夫
	docker run -it --rm -w /code/foo -t xxx:xxx cat hello.txt || :
