default:
	dot -Tsvg a.dot > a.svg
	dot -Tsvg b.dot > b.svg
	python ../go_dot.py `go list fmt` > fmt.dot
	dot -Tsvg fmt.dot > fmt.svg
	python ../go_dot.py `go list fmt encoding/json` > fmt2.dot
	dot -Tsvg fmt2.dot > fmt2.svg
	python ../go_dot.py `go list net/http` > net_http.dot
	dot -Tsvg net_http.dot > net_http.svg
	python ../go_dot.py `go list go/format go/importer` > go_tool.dot
	dot -Tsvg go_tool.dot > go_tool.svg
	# python ../go_dot.py `go list std` > std.dot
	# dot -Tsvg std.dot > std.svg

clean:
	rm -f *.svg

.PHONY: clean default
