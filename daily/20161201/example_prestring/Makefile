gencode:
	python 02simplified.py --name Github ./misc/github.json > ./output/github.go
	python 02simplified.py --name Smartstreet ./misc/smartstreet.json > ./output/smartstreet.go
	python 02simplified.py --name Person ./misc/person.json > ./output/person.go
	find ./output -name "*.go" | xargs gofmt -w
