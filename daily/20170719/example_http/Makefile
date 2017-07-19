%.output.json: %.go
	go run $< | gsed -r "s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[mGK]//g" > $@

main: $(shell ls *.go | sed 's/\.go$$/.output.json/g')

clean: *.output.json
	rm -f $^
