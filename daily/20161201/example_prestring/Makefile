gencode:
	mkdir -p output
	python 02*.py --name Github ./misc/github.json > ./output/github.go
	python 02*.py --name Smartstreet ./misc/smartstreet.json > ./output/smartstreet.go
	python 02*.py --name Person ./misc/person.json > ./output/person.go
	python 02*.py --name Article ./misc/article.json > ./output/article.go
	python 02*.py --name Tree ./misc/tree.json > ./output/tree.go
	python 02*.py --name Abcdez ./misc/abcdez.json > ./output/abcdez.go
	find ./output -name "*.go" | xargs gofmt -w
	mkdir -p output2
	python 08*.py --name Github ./misc/github.json > ./output2/github.go
	python 08*.py --name Smartstreet ./misc/smartstreet.json > ./output2/smartstreet.go
	python 08*.py --name Person ./misc/person.json > ./output2/person.go
	python 08*.py --name Article ./misc/article.json > ./output2/article.go
	python 08*.py --name Tree ./misc/tree.json > ./output2/tree.go
	python 08*.py --name Abcdez ./misc/abcdez.json > ./output2/abcdez.go
	find ./output2 -name "*.go" | xargs gofmt -w
