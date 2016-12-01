gencode:
	mkdir -p output
	python 02simplified.py --name Github ./misc/github.json > ./output/github.go
	python 02simplified.py --name Smartstreet ./misc/smartstreet.json > ./output/smartstreet.go
	python 02simplified.py --name Person ./misc/person.json > ./output/person.go
	find ./output -name "*.go" | xargs gofmt -w
	mkdir -p output2
	python 04goodname.py --name Github ./misc/github.json > ./output2/github.go
	python 04goodname.py --name Smartstreet ./misc/smartstreet.json > ./output2/smartstreet.go
	python 04goodname.py --name Person ./misc/person.json > ./output2/person.go
	find ./output2 -name "*.go" | xargs gofmt -w
