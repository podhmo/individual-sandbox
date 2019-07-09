default: 00 01 02 03

gen:
	$(MAKE) default | sed 's/dictknife/\n$$ dictknife/g'
00:
	dictknife cat person.json -o json
01:
	dictknife describe --minimum person.json -o json
02:
	dictknife describe --minimum -d 1 person.json -o json
03:
	dictknife describe --minimum -d 2 person.json -o json
