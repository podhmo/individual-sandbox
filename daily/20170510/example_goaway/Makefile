default: person person1 person2 person3 person4

person:
	mkdir -p dst/00/
	swagger2go src/00*.yaml --package=github.com/podhmo/person --position=./dst/00 --file=person.go --ref "#/definitions/person"
	goimports -w dst/00/person/*.go

person1:
	mkdir -p dst/01/
	swagger2go src/01*.yaml --package=github.com/podhmo/person --position=./dst/01 --file=person.go
	goimports -w dst/01/person/*.go

person2:
	mkdir -p dst/02/
	swagger2go src/02*.yaml --package=github.com/podhmo/person --position=./dst/02 --file=person.go
	goimports -w dst/02/person/*.go

person3:
	mkdir -p dst/03/
	swagger2go src/03*.yaml --package=github.com/podhmo/person --position=./dst/03 --file=person.go
	goimports -w dst/03/person/*.go

person4:
	mkdir -p dst/04/
	swagger2go src/04*.yaml --package=github.com/podhmo/person --position=./dst/04 --file=person.go
	goimports -w dst/04/person/*.go

person5:
	mkdir -p dst/05/
	swagger2go src/05*.yaml --package=github.com/podhmo/person --position=./dst/05 --file=person.go
	goimports -w dst/05/person/*.go

person6:
	mkdir -p dst/06/
	swagger2go src/06*.yaml --package=github.com/podhmo/person --position=./dst/06 --file=person.go
	goimports -w dst/06/person/*.go

person7:
	mkdir -p dst/07/
	swagger2go --writer=./writer.py:MyWriter src/07*.yaml --package=github.com/podhmo/person --position=./dst/07 --file=person.go
	goimports -w dst/07/person/*.go

person8:
	mkdir -p dst/08/
	swagger2go --walker=./walker.py:MyWalker --writer=./writer.py:MyWriter src/08*.yaml --package=github.com/podhmo/person --position=./dst/08 --file=person.go
	goimports -w dst/08/person/*.go

