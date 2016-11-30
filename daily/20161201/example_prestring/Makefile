gencode:
	python 01transform.py --name Github ./misc/github.json > ./output/github.go
	python 01transform.py --name Smartstreet ./misc/smartstreet.json > ./output/smartstreet.go
	python 01transform.py --name Person ./misc/person.json > ./output/person.go
	find ./output -name "*.go" | xargs gofmt -w
